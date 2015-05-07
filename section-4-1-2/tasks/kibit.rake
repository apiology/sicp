task :kibit do
  sh 'lein kibit'
end

task :update_lein_kibit do
  sh 'cd ~/src/lein-kibit && lein install'
end

task :update_kibit do
  sh 'cd ~/src/kibit && lein install'
end
