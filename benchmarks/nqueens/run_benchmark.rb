#!/usr/local/bin/ruby
file = ARGV[0]

LOWERBOUND = 8

TRIALS = 5

# the second argument, if it exists, determines the upper bound
if ARGV[1].nil? then
  UPPERBOUND = 12
else
  UPPERBOUND = ARGV[1].to_i
end

puts "script\t\"#{file}\""

unless ARGV[2].nil?
  puts "header\t#{ARGV[2]}"
end

LOWERBOUND.upto(UPPERBOUND) do |n|
  best = 0
  TRIALS.times {
    unless best > 60*1000*2 then
      run = `timeout 10m sml #{file} #{n}`.lines[-1].to_i
      if best = 0 || run < best then best = run end
    end
  }
  best += 1 # stabilize on log-axis
  puts "point\t#{n}\t#{best}"
end

