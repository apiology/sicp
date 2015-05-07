BIKESHED_OPTIONS = '-v -m 200'
task :bikeshed do
  sh "lein do bikeshed #{BIKESHED_OPTIONS}"
end
