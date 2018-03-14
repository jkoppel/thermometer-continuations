#!/usr/local/bin/ruby
file = ARGV[0]

LOWERBOUND = 4
UPPERBOUND = 12

TRIALS = 5

# we use the presence of a second command-line parameter
# as a switch to a LaTeX-friendly output
if ARGV[1].nil? then
  puts file
else
  puts "\\header{#{ARGV[1]}}"
end
LOWERBOUND.upto(UPPERBOUND) do |n|
  tot = 0.0
  TRIALS.times { tot += `sml #{file} #{n}`.lines[-1].to_i }
  avg = tot / TRIALS
  if ARGV[1].nil? then
    puts "#{n}: #{avg}"
  else
    puts "\\point{#{n}}{#{avg}}"
  end
end

