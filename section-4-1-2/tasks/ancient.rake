task :ancient do
  #
  # 'lein ancient' doesn't return an error code, but I'd like one so
  # that I can fail CI when we start falling out of date
  #
  # 'lein ancient' also doesn't let me dasily boot bad versions
  #
  sh 'if lein ancient 2>&1 ' \
     "| grep -v '^all artifacts are up-to-date.' " \
     "| grep -v 'org.clojure/core.typed.*0.2.87' " \
     "| grep -v '^$' " \
     "| grep '^'; then exit 1; else exit 0; fi"
  sh 'if lein ancient profiles ' \
     "| grep -v '^all artifacts are up-to-date.' " \
     "| grep -v -- '-- ~/.lein/profiles.clj' " \
     "| grep -v '^$' " \
     "| grep '^'; then exit 1; else exit 0; fi"
end
