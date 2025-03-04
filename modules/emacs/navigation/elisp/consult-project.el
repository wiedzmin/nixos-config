(use-package consult-project-extra
  :after project
  :bind
  (:map custom-projects-map
        ("h" . consult-project-extra-find)))
