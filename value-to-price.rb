#!/usr/bin/env ruby
require 'json'
require './jester'

players = JSON.parse(ARGF.read)['playerList'].map {|pl| Player.new(pl)}

min_ppg = players.min_by(&:ppg).ppg
max_ppg = players.max_by(&:ppg).ppg

min_s   = players.min_by(&:s).s
max_s   = players.max_by(&:s).s

min_v   = players.min_by(&:value).value
max_v   = players.max_by(&:value).value

def normalizedScore(min, max, scoreToNormalize)
	((((100-0)*(scoreToNormalize-min))/(max-min))+0.0)
end
puts "name,ppg,nppg,s,ns,v,nv"

players.each do |player|
  puts [
    player.name,
    player.ppg,
    normalizedScore(min_ppg, max_ppg, player.ppg),
    player.s,
    normalizedScore(min_s, max_s, player.s),
    player.value,
    normalizedScore(min_v, max_v, player.value)
  ].join(",")
end
