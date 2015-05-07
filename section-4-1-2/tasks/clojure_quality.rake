task :clojure_quality do
  # XXX reduce line size
  sh 'lein do kibit, eastwood'
  # sh "lein bikeshed #{BIKESHED_OPTIONS}" # I don't have enough
  # docstrings for this yet
end
