import os # TODO: consider creating separate patch with stdlib imports (for deduplication)
import re
import subprocess


field_regex_mappings = {
    "login": r"^login: +(.*)$",
}

# TODO: add logging
def collect_entries(path):
    entries = []
    entries_task = subprocess.Popen("find . -type f -name '*.gpg'", cwd=path,
                                    shell=True, stdout=subprocess.PIPE) # FIXME: debug and change to `fd`
    result = entries_task.wait()
    if result == 0:
        entries.extend([entry[2:-4] for entry in entries_task.stdout.read().decode().split("\n")])
    else:
        log_error(entries_task.stderr.read().decode())
    return entries


def read_entry(path):
    pass_entry_task = subprocess.Popen(f"pass show {path}", shell=True, stdout=subprocess.PIPE)
    result = pass_entry_task.wait()
    if result != 0:
        return None
    return pass_entry_task.stdout.read().decode().strip().split("\n")


def extract_entry_name(path):
    return os.path.split(path)[-1]


def extract_skip_prefix(line, prefix):
    if line is not None:
        return line[len(prefix):]
    else:
        return None


def extract_specific_line(entry, line):
    if len(entry) > line:
        return entry[line]
    else:
        return None


def extract_by_regex(entry, regex):
    matcher = re.compile(regex)
    if matcher.groups != 1:
        raise ValueError(
            'Provided regex "{regex}" must contain a single '
            "capture group for the value to return.".format(regex=regex)
        )
    for line in entry:
        match = matcher.match(line)
        if match:
            return match.group(1)
    return None
