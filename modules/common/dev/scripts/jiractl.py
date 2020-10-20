from datetime import date, datetime, timedelta
import json
import subprocess
import sys

from jira import JIRA
import redis
import pytz

from pystdlib.uishim import get_selection, notify, URGENCY_NORMAL, URGENCY_CRITICAL
from pystdlib import shell_cmd


r = redis.Redis(host='localhost', port=6379, db=0)
identities = json.loads(r.get("jira/identities"))


client = None
issues = None


def select_identity():
    identity = get_selection(identities.keys(), 'identity', case_insensitive=True,
                             lines=5, font="@wmFontDmenu@")
    return identities.get(identity, None)


def get_client(server=None, auth=None):
    return JIRA(server=server, auth=auth)


def get_issues(client, project, active_only=True, mine_only=True):
    assignee_clause = "and assignee = currentUser()" if mine_only else ""
    active_clause = "and status in (1,3,4)" if active_only else "" # FIXME: retrieve dynamically
    # (https://developer.atlassian.com/cloud/jira/platform/rest/v2/api-group-workflow-statuses/#api-rest-api-2-status-get)
    # https://jira.readthedocs.io/en/master/api.html#jira.JIRA.find
    return client.search_issues(f"project={project} {assignee_clause} {active_clause} order by created asc",
                                maxResults=100)


def select_issue(issues):
    issues_map = {}
    for issue in issues:
        issues_map[issue.key + " | " + issue.fields.summary + " | " + issue.fields.created] = issue
    selected_issue = get_selection(issues_map.keys(), 'issue', case_insensitive=True,
                                   lines=5, font="@wmFontDmenu@")
    return issues_map.get(selected_issue, None)


# TODO: (global) make and maintain known urls registry, make webjumps and searches its subsets
def issue_view_in_browser(issue):
    # FIXME: access server value in more straightforward manner
    shell_cmd(f"qutebrowser {client.options['server']}/browse/{issue.key}", oneshot=True)


def issue_log_work(issue):
    unloddeg_dates = [datetime.strftime(d, "%d/%m/%y") for d in issue_get_unlogged_dates(issue)]
    date_to_log = get_selection(sorted(unloddeg_dates), 'date', case_insensitive=True,
                                lines=15, font="@wmFontDmenu@")
    if not date_to_log:
        notify("[jira]", f"Cancelled logging work for `{issue.key}`", urgency=URGENCY_CRITICAL)
        sys.exit(1)

    started = datetime.combine(datetime.strptime(date_to_log, "%d/%m/%y"), datetime.now().time())
    amount, comment = None, None
    while not comment:
        result = worklog_get_amount_and_comment()
        if result:
            amount, comment = result.split("|")[:-1]
            if not comment:
                notify("[jira]", f"comment in worklog is mandatory'", urgency=URGENCY_CRITICAL)
        else:
            break
    if amount and comment:
        tz = pytz.timezone("@systemTimeZone@")
        started_with_tz = tz.localize(started)
        wlog = client.add_worklog(issue, timeSpent=amount, started=started_with_tz, comment=comment)
        notify("[jira]", f"Logged `{amount}` of work for `{issue.key}` at {date_to_log}'", urgency=URGENCY_NORMAL)


def worklog_get_amount_and_comment():
    task = subprocess.Popen(f'yad --title="Log work" --form --field="amount":CB "1h\!2h\!3h\!4h\!1d" --field=comment:TXT',
                            shell=True, stdout=subprocess.PIPE)
    return task.stdout.read().decode()


def issue_get_unlogged_dates(issue):
    logged = []
    for issue in issues:
        worklogs = client.worklogs(issue=issue.id)
        for wlog in worklogs:
            logged.append(get_date_from_string(wlog.started))

    unlogged = []
    d = get_date_from_string(issue.fields.created)
    while d <= date.today():
        if d not in logged:
            unlogged.append(d)
        d += timedelta(days=1)

    return unlogged


def get_date_from_string(str):
    return datetime.strptime(str.split(".")[0], "%Y-%m-%dT%H:%M:%S").date()



def issue_make_transition(issue):
    # NOTE: for API details refer to chapter 2.1.7 (Transitions) of documentation
    notify("[jira]", f"`Change status` not implemented yet", urgency=URGENCY_CRITICAL)
    sys.exit(1)


def issue_view_git_branch(task):
    # TODO: ensure correct vpn state, parameterize vpn name
    notify("[jira]", f"`View git branch` not implemented yet", urgency=URGENCY_CRITICAL)
    sys.exit(1)


OPS = {
    "View in browser": issue_view_in_browser,
    "Log work": issue_log_work,
    "Change status": issue_make_transition,
    "View git branch": issue_view_git_branch
}


identity = select_identity()
if not identity:
    notify("[jira]", "invalid identity", urgency=URGENCY_CRITICAL)
    sys.exit(1)

client = get_client(server=identity["meta"]["JIRA_SERVER"], auth=tuple(identity["creds"]))
issues = get_issues(client, identity["meta"]["PROJECT"])

issue = select_issue(issues)
if not issue:
    notify("[jira]", "No issue selected", urgency=URGENCY_NORMAL)
    sys.exit(0)

op = get_selection(OPS.keys(), '>', case_insensitive=True,
                   lines=5, font="@wmFontDmenu@")
if not op:
    notify("[jira]", "No operation selected", urgency=URGENCY_NORMAL)
    sys.exit(0)
if op not in OPS:
    notify("[jira]", "Invalid operation `{op}`", urgency=URGENCY_NORMAL)
    sys.exit(0)

OPS[op](issue)
