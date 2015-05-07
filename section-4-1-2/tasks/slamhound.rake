# XXX contribute this back to slamhound
task :slamhound do
  sh 'find . -name \*.clj | grep -v project.clj | xargs lein slamhound'
  sh "if git status --porcelain | grep '^'; then exit 1; else exit 0; fi"
end
