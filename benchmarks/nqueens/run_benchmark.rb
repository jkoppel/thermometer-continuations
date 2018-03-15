#!/usr/local/bin/ruby
file = ARGV[0]

TRIALS = 5

# the second and third arguments determine the lower and upper bound
if ARGV[1].nil? then
  LOWERBOUND = 8
else
  LOWERBOUND = ARGV[1].to_i
end

if ARGV[2].nil? then
  UPPERBOUND = 12
else
  UPPERBOUND = ARGV[2].to_i
end

puts "script\t\"#{file}\""

unless ARGV[3].nil?
  puts "header\t#{ARGV[3]}"
end

LOWERBOUND.upto(UPPERBOUND) do |n|
  best = 0
  TRIALS.times {
    run = `timeout 10m sml #{file} #{n}`.lines[-1].to_i
    if best == 0 || run < best then best = run end
  }
  best += 1 # stabilize on log-axis
  puts "point\t#{n}\t#{best}"
end

