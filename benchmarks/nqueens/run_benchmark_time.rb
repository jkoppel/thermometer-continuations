#!/usr/local/bin/ruby
# Use this to time the Curry implementation

require 'open3'

cmd = ARGV[0]

LOWERBOUND = 4

TRIALS = 5

# the second argument, if it exists, determines the upper bound
if ARGV[1].nil? then
  UPPERBOUND = 12
else
  UPPERBOUND = ARGV[1].to_i
end

puts "script\t\"#{cmd}\""

unless ARGV[2].nil?
  puts "header\t\"#{ARGV[2]}\""
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
  avg = 1 + tot / TRIALS
  puts "point\t#{n}\t#{avg}"
end

