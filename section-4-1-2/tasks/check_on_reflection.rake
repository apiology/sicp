# XXX contribute this back to lein check
task :check_on_reflection do
  sh 'if lein check 2>&1 ' \
     "| grep 'Reflection warning' " \
     "| grep 'groceries_server/' " \
     "| grep '^'; then exit 1; else exit 0; fi"
end
