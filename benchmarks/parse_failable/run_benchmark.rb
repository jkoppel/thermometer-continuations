#!/usr/local/bin/ruby
file = ARGV[0]
scale = ARGV[1].to_f


VALUES = [1000, 10000, 50000, 100000, 500000, 1000000, 5000000]

TRIALS = 5


puts "#{file} #{scale}"
VALUES.each do |n|
  tot = 0.0
  TRIALS.times { tot += `sml #{file} #{n} #{(n*scale).to_i}`.lines[-1].to_i }
  avg = tot / TRIALS
  puts "#{n}: #{avg}"
end

