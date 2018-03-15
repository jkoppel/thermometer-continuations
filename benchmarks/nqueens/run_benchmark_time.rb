#!/usr/local/bin/ruby
# Use this to time the Curry implementation

require 'open3'

cmd = ARGV[0]

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

TRIALS = 5

puts "script\t\"#{cmd}\""

unless ARGV[3].nil?
  puts "header\t#{ARGV[3]}"
end

LOWERBOUND.upto(UPPERBOUND) do |n|
  best = 0

  TRIALS.times do
     _, stderr, _ = Open3.capture3("bash -c \"time #{cmd} #{n}\"")
    s = stderr.lines.join
    #p s
    m = s.match(/user\s+(\d+)m(\d+.\d+)s/)
    run = (m[1].to_i * 60 + m[2].to_f) * 1000
    if best == 0 || run < best then best = run end
  end
  best += 1 # stabilize on log-axis
  puts "point\t#{n}\t#{best}"
end

