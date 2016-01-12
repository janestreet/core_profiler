let package_name = "core_profiler"

let sections =
  [ ("lib",
    [ ("built_lib_core_profiler", None)
    ; ("built_lib_core_profiler_disabled", None)
    ; ("built_lib_core_profiler_offline_tool", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_core-profiler-dump-metrics", Some "core-profiler-dump-metrics")
    ; ("built_exec_core-profiler-tool", Some "core-profiler-tool")
    ],
    [])
  ]
