# XXX add ratcheting part of this to quality gem
# XXX create lein version of quality gem
# XXX add this idea to lein cloverage
task :cloverage do
  # XXX doesn't capture failure, so I have to do 'test' as well as
  # 'cloverage'...
  coverage =
    `lein cloverage | grep '^Forms covered:' | cut -d' ' -f3`.strip.to_f
  old_coverage = `cat metrics/cloverage_high_water_mark`.strip.to_f
  if coverage < old_coverage
    fail "Coverage dropped to #{coverage} from #{old_coverage}"
  elsif coverage == old_coverage
    puts "Coverage maintained at #{coverage}"
  else
    puts "Coverage ratcheted up to #{coverage} from #{old_coverage}"
    File.open('metrics/cloverage_high_water_mark', 'w') do |file|
      file.write(coverage)
    end
  end
end
