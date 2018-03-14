#!/usr/local/bin/ruby
file = ARGV[0]

LOWERBOUND = 4

TRIALS = 5

# the second argument, if it exists, determines the upper bound
if ARGV[1].nil? then
  UPPERBOUND = 12
else
  UPPERBOUND = ARGV[1].to_i
end

puts "script\t\"#{file}\""

unless ARGV[2].nil?
  puts "header\t\"#{ARGV[2]}\""
end

LOWERBOUND.upto(UPPERBOUND) do |n|
  tot = 0.0
  TRIALS.times { tot += `sml #{file} #{n}`.lines[-1].to_i }
  avg = 1 + tot / TRIALS
  puts "point\t#{n}\t#{avg}"
end

