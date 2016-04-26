#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"core_profiler"
  [ oasis_lib "core_profiler"
  ; oasis_lib "core_profiler_disabled"
  ; oasis_lib "core_profiler_offline_tool"
  ; file "META" ~section:"lib"
  ; oasis_exe "core-profiler-dump-metrics" ~dest:"core-profiler-dump-metrics"
  ; oasis_exe "core-profiler-tool" ~dest:"core-profiler-tool"
  ]
