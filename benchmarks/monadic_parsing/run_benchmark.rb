#!/usr/local/bin/ruby
file = ARGV[0]

VALUES = [40]

TRIALS = 30


puts "#{file}"
VALUES.each do |n|
  tot = 0.0
  6.upto(TRIALS-1) do |i|
    t = `sml #{file} #{n} #{i}`.lines[-1].to_i
    tot += t
    puts "Trial #{i}: #{t}" 
  end
  avg = tot / TRIALS
  puts "#{n}: #{avg}"
end

