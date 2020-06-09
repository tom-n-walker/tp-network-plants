## download data


download_plan <- drake_plan(
  #community
  community_download = target(
    get_file(
      node = "f3knq",
      remote_path = "Community",
      file = "transplant.sqlite",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "f3knq",
        remote_path = "Community",
        file = "transplant.sqlite",
        path = "data"
      )
    )
  )
)