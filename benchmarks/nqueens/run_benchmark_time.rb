#!/usr/local/bin/ruby
# Use this to time the Curry implementation

require 'open3'

cmd = ARGV[0]

LOWERBOUND = 4
UPPERBOUND = 12

TRIALS = 5

# we use the presence of a second command-line parameter
# as a switch to a LaTeX-friendly output
if ARGV[1].nil? then
  puts cmd
else
  puts "\\header{#{ARGV[1]}}"
end
LOWERBOUND.upto(UPPERBOUND) do |n|
  tot = 0.0

  TRIALS.times do
     _, stderr, _ = Open3.capture3("bash -c \"time #{cmd} #{n}\"")
    s = stderr.lines.join
    #p s
    m = s.match(/user\s+(\d+)m(\d+.\d+)s/)
    tot += (m[1].to_i * 60 + m[2].to_f) * 1000
  end
  avg = tot / TRIALS
  if ARGV[1].nil? then
    puts "#{n}: #{avg}"
  else
    puts "\\point{#{n}}{#{avg}}"
  end
end

