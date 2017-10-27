#!/usr/local/bin/ruby
file = ARGV[0]

LOWERBOUND = 4
UPPERBOUND = 12

TRIALS = 5


puts file
LOWERBOUND.upto(UPPERBOUND) do |n|
  tot = 0.0
  TRIALS.times { tot += `sml #{file} #{n}`.lines[-1].to_i }
  avg = tot / TRIALS
  puts "#{n}: #{avg}"
end

