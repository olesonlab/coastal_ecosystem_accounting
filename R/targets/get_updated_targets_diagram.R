get_updated_targets_diagram <- list(
  tar_target(
    targets_pipeline_diagram,
    generate_targets_diagram()
  ),
  tar_target(
    exported_targets_widget,
    export_widget_as_html(
      widget = targets_pipeline_diagram, 
      file_path = create_file_path(
        dir = "app/static/",
        file_name = paste0(
          get_todays_date(),
          "targets_widget"
        ), 
        ext = "html"
      )
    )
  )
)