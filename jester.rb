#!/usr/bin/env ruby 

require 'rubygems'
require 'capybara'
require 'capybara/poltergeist'
require 'ostruct'
require 'set'
require 'json'

######
# You don't need to do all the javascript magic below. You can just make 2 requests with curl:
#
# To get the DraftGroupId
# curl -d "SportId=8" 'https://www.draftkings.com/lineup/getupcomingcontestinfo'
#
# Use the DraftGroupId to get the list of all available players
# curl 'https://www.draftkings.com/lineup/getavailableplayers?draftGroupId=7428'

# Other urls:
# 
# Returns lineup ids
# https://www.draftkings.com/lineup/getrecentlineupsforuser
#
# Returns lineups with players
# https://www.draftkings.com/lineup/getlineupswithplayersforuser
# 
# Your lineups in csv
# https://www.draftkings.com/mycontests/historycsv
  
def update_players
  include Capybara::DSL

  Capybara.default_driver = :poltergeist
  Capybara.javascript_driver = :poltergeist
  Capybara.run_server = false

  Capybara.register_driver :poltergeist do |app|
    driver = Capybara::Poltergeist::Driver.new(app, {js_errors: false, debug: true, :default_wait_time => 10, :timeout => 90, phantomjs_options: ['--ignore-ssl-errors=true']})
    driver.headers = { 'User-Agent' => 'Mozilla/5.0 (iPhone; U; CPU iPhone OS 5_0 like Mac OS X) AppleWebKit/534.46 (KHTML, like Gecko) Mobile/9A334 Safari/7534.48.3' }
    driver
  end
  
  Capybara.app_host = "https://www.draftkings.com"
  visit("/account/sitelogin/false")
  sleep(4)
  fill_in("Username", :with => "draftkings@marcelmolina.com")
  fill_in("Password", :with => "Talwin21")
  page.find('#loginButton').trigger('click')
  sleep(4)
  visit("/lineup/createlineup")
  sleep(4)
  page.find(:xpath, "//a//span[text()='NFL']").trigger('click')
  sleep(4)
  page.find("div#lobbyInfo li:nth-child(1) a").trigger('click')
  sleep(4)
  playerC = page.evaluate_script("playerC")
  players = playerC["playerList"].map {|player| 
    Player.new(player) 
  }
  by_position = players.group_by {|p| p.struct.pn }
  # keys: => ["WR", "RB", "QB", "TE", "DST"]
  by_status = players.group_by{|p| p.struct.i }
  # keys: => ["", "O", "Q", "P", "D", "IR"]
  by_team_id = players.group_by {|p| p.struct.tid }
  by_salary = players.group_by {|p| p.struct.s }
  
  {players: players, by_position: by_position, by_status: by_status, by_team_id: by_team_id, by_salary: by_salary}
end

 # ap = JSON.parse(IO.read('draftkings-ffn-10-14-2015.json'))['players'].map {|pl| Player.new(pl)};1
 # ap = AllPlayers.new(ap);1

 # dkp = JSON.parse(IO.read('draftkings-players-10-13-2015.json'))['playerList'].map {|pl| Player.new(pl)};1
 # dkap = AllPlayers.new(dkp);1
 
class AllPlayers
  attr_reader :players
  RANK_BY = :value
  
  RB_PERCENTAGE = 30
  # RB_PERCENTAGE = 10
  # WR_PERCENTAGE = [20, 10]
  WR_PERCENTAGE = [40, 5]
  RB_WR_PERCENTAGE = 5
  QB_PERCENTAGE = 40
  TE_PERCENTAGE = 15
  FLEX_PERCENTAGE = 40
  TE_FLEX_PERCENTAGE = 10
  QB_RB_WR_PERCENTAGE = 1.25
  QB_RB_WR_TE_FLEX_PERCENTAGE = 5
  DEF_PERCENTAGE = 50
  # RB_PERCENTAGE  = RB_WR_PERCENTAGE = QB_PERCENTAGE = TE_PERCENTAGE =  FLEX_PERCENTAGE = TE_FLEX_PERCENTAGE = QB_RB_WR_PERCENTAGE = QB_RB_WR_TE_FLEX_PERCENTAGE = DEF_PERCENTAGE = 50
  # WR_PERCENTAGE = [50, 50]
  
  def initialize(players)
    players.delete_if {|pl| pl.points.zero? || pl.value.zero? }
    @players = players
    @count_by_position_cache = {}
  end
  
  FLEX_POSITIONS = Set.new(["RB", "WR", "TE"])
  
  def alternatives_for(player, options = {})
    options[:flex] = false if options[:flex].nil?
    flex = options[:flex]
    budget = options[:budget] || player.s
    sort_metric = options[:metric] || :points
    players.select do |pl|
      if flex
        FLEX_POSITIONS.include?(pl.pn) && pl.s <= budget && player != pl
      else
        pl.pn == player.pn && pl.s <= budget && pl != player
      end
    end.sort_by(&sort_metric).reverse
  end
  
  def all_named(name)
    players.select do |pl|
      pl.name[name]
    end
  end
  
  def [](name)
    case name
    when String
      players.detect {|pl| pl.name[name]}
    when Array
      name.map {|n| self[n]}
    end
  end
  
  def possible_rbs
    @possible_rbs ||= players.select {|pl| pl.pn == 'RB' }.rsort_by(&RANK_BY).take_percent(RB_PERCENTAGE).permutation(2).map do |combo|
      PossiblePlayers.new(combo)
    end.rsort_by(&RANK_BY).take_percent(RB_PERCENTAGE).uniq
  end
  
  def possible_wrs
    @possible_wrs ||= players.select {|pl| pl.pn == 'WR' }.rsort_by(&RANK_BY).take_percent(WR_PERCENTAGE[0]).permutation(3).map do |combo|
      PossiblePlayers.new(combo)
    end.sort_by(&RANK_BY).reverse.take_percent(WR_PERCENTAGE[1]).uniq
  end
  
  def possible_rbs_wrs
    return @rbs_wrs unless (@rbs_wrs.nil? || @rbs_wrs.empty?)
    
    @rbs_wrs = Set.new
    possible_rbs.each do |possible_rb|
      possible_wrs.each do |possible_wr|
        @rbs_wrs << (possible_rb + possible_wr)
      end
    end
    
    @rbs_wrs = Set.new(@rbs_wrs.rsort_by(&RANK_BY).take_percent(RB_WR_PERCENTAGE))
    @rbs_wrs
  end
  
  def possible_qbs
    players.select {|pl| pl.pn == 'QB'}.rsort_by(&RANK_BY).take_percent(QB_PERCENTAGE).map do |pl|
      PossiblePlayers.new([pl])
    end
  end
  
  def possible_tes
    players.select {|pl| pl.pn == 'TE'}.rsort_by(&RANK_BY).take_percent(TE_PERCENTAGE).map do |pl|
      PossiblePlayers.new([pl])
    end
  end
  
  def possible_flex
    players.select {|pl| pl.pn == 'TE' || pl.pn == 'WR' || pl.pn == 'RB'}.rsort_by(&RANK_BY).take_percent(FLEX_PERCENTAGE).map do |pl|
      PossiblePlayers.new([pl])
    end
  end
  
  def possible_tes_flex
    return @tes_flex unless (@tes_flex.nil? || @tes_flex.empty?)
    @tes_flex = Set.new
    possible_tes.each do |possible_te|
      possible_flex.each do |possible_flex|
        @tes_flex << (possible_te + possible_flex)
      end
    end
    
    @tes_flex = Set.new(@tes_flex.rsort_by(&RANK_BY).take_percent(TE_FLEX_PERCENTAGE))
    @tes_flex
  end
  
  def possible_qb_wrs_rbs
    return @qbs_rbs_wr unless (@qbs_rbs_wr.nil? || @qbs_rbs_wr.empty?)
    @qbs_rbs_wr = Set.new
    possible_qbs.each do |possible_qb|
      possible_rbs_wrs.each do |possible_wr_rb|
        ros = possible_qb + possible_wr_rb
        if ros.under_budget?
          @qbs_rbs_wr << ros
        end
      end
    end
    
    @qbs_rbs_wr = Set.new(@qbs_rbs_wr.rsort_by(&RANK_BY).take_percent(QB_RB_WR_PERCENTAGE))
    @qbs_rbs_wr
  end
  
  def possible_qb_wr_rb_te_flex
    return @qb_wr_rb_te_flex unless (@qb_wr_rb_te_flex.nil? || @qb_wr_rb_te_flex.empty?)
    defense = possible_def
    
    cheapest_defense = defense.min_by(&:cost)
    
    @qb_wr_rb_te_flex = Set.new
    possible_qb_wrs_rbs.each do |possible_1|
      possible_tes_flex.each do |possible_2|
        ppl = (possible_1 + possible_2)
        if ppl.under_budget? && ppl.remaining_budget > cheapest_defense.cost
          @qb_wr_rb_te_flex <<  ppl
        end
      end
    end
    
    @qb_wr_rb_te_flex = Set.new(@qb_wr_rb_te_flex.rsort_by(&RANK_BY).take_percent(QB_RB_WR_TE_FLEX_PERCENTAGE))
    @qb_wr_rb_te_flex
  end
  
  def best_possible_rosters
    return @rosters unless (@rosters.nil? || @rosters.empty?)
    
    @rosters = Set.new
    
    defense = possible_def
    
    cheapest_defense = defense.min_by(&:cost)
    
    possible_qb_wr_rb_te_flex.each do |possible_1|
      next if possible_1.remaining_budget < cheapest_defense.cost
      possible_def.each do |possible_2|
        pp = (possible_1 + possible_2)
        if pp.under_budget?
          @rosters << pp
        end
      end
    end
    
    @rosters = com.andbutso.jester.Rosters.new(Set.new(@rosters))
    @rosters
  end
  
  def possible_def
    players.select {|pl| pl.pn == 'DST' || pl.pn == 'DEF'}.rsort_by(&RANK_BY).take_percent(DEF_PERCENTAGE).map do |pl|
      PossiblePlayers.new([pl])
    end
  end
end

class Rosters
  attr_reader :rosters, :count_by_position_cache, :rosters_by_player_per_position_cache
  
  def initialize(rosters)
    @rosters = Set.new(rosters)
    @count_by_position_cache = {}
    @rosters_by_player_per_position_cache = {}
  end
  
  def distribution_by_position(position = "QB", min_count = 1)
    key = [position, min_count]
    return count_by_position_cache[key] if count_by_position_cache[key]
    
    counts = {}
    rosters.each do |roster|
      roster.players.select {|pl| pl.pn == position}.each do |player|
        counts[player] ||= 0
        counts[player] += 1
      end
    end
    
    count_by_position_cache[key] = counts.select { |k,v|
      v >= min_count
    }.sort_by {|k,v| v}
    
    count_by_position_cache[key]
  end
  
  def rosters_by_player_per_position
    return rosters_by_player_per_position_cache unless rosters_by_player_per_position_cache.empty?
    
    rosters.each do |roster|
      roster.players.each do |player|
        rosters_by_player_per_position_cache[player.pn] ||= {}
        rosters_by_player_per_position_cache[player.pn][player.name] ||= Set.new
        rosters_by_player_per_position_cache[player.pn][player.name] << roster
      end
    end
    
    rosters_by_player_per_position_cache.keys.each do |position|
      rosters_by_player_per_position_cache[position].keys.each do |name|
        r = rosters_by_player_per_position_cache[position][name]
        rosters_by_player_per_position_cache[position][name] = com.andbutso.jester.Rosters.new(r)
      end
    end
    
    rosters_by_player_per_position_cache
  end
  
  def by_points
    rsort_by(&:points)
  end
  
  def by_value
    rsort_by(&:value)
  end
  
  def by_cost
    rsort_by(&:cost)
  end
  
  def by_diversity_and_points
    best = by_points.first
    sort_by {|r| [-r.distance(best), -r.points] }
  end
  
  def by_diversity
    best = by_points.first
    sort_by {|r| -r.distance(best)}
  end
  
  def with_player(player)
    Rosters.new(rosters.select do |roster|
      roster.players.include?(player)
    end)
  end
  
  def excluding_player(player)
    Rosters.new(rosters.select do |roster|
      !roster.players.include?(player)
    end)
  end
  
  def min_max_by(metric = :points)
    [rosters.max_by(&metric), rosters.min_by(&metric)]
  end
    
  def method_missing(method, *args, &block)
    if rosters.respond_to?(method)
      rosters.send(method, *args, &block)
    else
      super
    end
  end
end

def merge(dk, ffn)
  dkplayers = dk['playerList']
  dkplayers_by_name = dkplayers.group_by {|dkp| dkp['ln'] }
  ffn['players'].each do |ffnp|
    if matches = dkplayers_by_name[ffnp['name']]
      case matches.size
      when 1
        merge_player(matches.first, ffnp)
      when 0
        next
      else
        matches.each do |match|
          merge_player(match, ffnp)
        end
      end
    end
  end
end

$did_not_match = Set.new
def merge_player(dkp, ffnp)
  if dkp['pn'] == ffnp['position'] && dkp['s'] == ffnp['salary'].to_i
    projections = ffnp['projections']
    
    dkp['projection_low'] = projections['conservative']['projectedPoints']
    dkp['projection_consensus'] = projections['consensus']['projectedPoints']
    dkp['projection_high'] = projections['aggressive']['projectedPoints']
    dkp['bfb_low'] = projections['conservative']['bangForYourBuckScore']
    dkp['bfb_consensus'] = projections['consensus']['bangForYourBuckScore']
    dkp['bfb_high'] = projections['aggressive']['bangForYourBuckScore']
    $did_not_match.delete(ffnp)
  else
    $did_not_match << ffnp
  end
end

class PossiblePlayers  
  BUDGET = 50_000
  POSITIONS = 9
  
  attr_reader :players, :id
  def initialize(players)
    @points = {}
    @players = Set.new(players)
    @id = players.map(&:pid).sort.join("|")
  end
  
  def [](str)
    players.detect {|pl| pl.name[str]}
  end
  
  MIN_FOR_POSITION = {
    'TE' => 1,
    'RB' => 2,
    'WR' => 3
  }
  
  def summary
    p = players.first.ffn? ? projections.join(" / ") : points
    s = "Points: #{p}\n"
    s << "Cost: #{cost}\n\n"
    
    name_padding = players.map {|p| p.name.size}.max
    pl = players.rsort_by(&:points).map do |player|
      player.name.rjust(name_padding) + ": #{player.points} / #{player.salary} #{player.position}"
    end
    
    s << pl.join("\n")
  end
  
  def alternatives_for(name, all_players, options = {})
    player = self[name]
    
    if options[:flex].nil? && player.flex_position?
      remaining_players = players - [player]
      
      by_position = remaining_players.select(&:flex_position?).group_by(&:pn)
      room_at_position = MIN_FOR_POSITION.all? do |position, min| 
        spots = by_position[position]
        !spots.nil? && spots.size == min 
      end
      
      options[:flex] = room_at_position
    end
    
    options[:budget] ||= (remaining_budget + self[name].s)
    all_players.alternatives_for(self[name], options)
  end
  
  def alternatives(all_players, options = {})
    alternates = players.inject({}) do |alternates_by_player, player|
      alternates_by_player[player] = alternatives_for(player.name, all_players, options).sort_by(&:points).reverse.first(6)
      alternates_by_player
    end
    
    foo = Set.new
    alternates.map do |player, replacements|
      alternates.map do |player2, replacements2|
        next if player == player2
        replacements.each_with_index do |replacement1|
          replacements2.each_with_index do |replacement2|
            r = swap(player, replacement1).swap(player2, replacement2)
            if r.under_budget? && r.full?
              foo << r
            end
          end
        end
      end
    end
    
    Rosters.new(foo)
  end
  
  def improve_on(name, all_players, options = {}, index = 0)
    alternatives = alternatives_for(name, all_players, options).select {|pl| !players.include?(pl)}
    swap(self[name], alternatives[index])
  end
  
  def improvements(all_players, options = {})
    players.map do |player|
      improve_on(player.name, all_players, {metric: :points}.merge(options))
    end.sort_by(&:points).reverse
  end
  
  def improve(all_players, options = {})
    improvements(all_players, options)[0]
  end
  
  def improve!(all_players, options = {})
    outcome = improve(all_players, options)
    if outcome.points <= points
      self
    else
      outcome.improve!(all_players, options)
    end
  end
  
  def swap(old, newp)
    PossiblePlayers.new(players - [old] + [newp])
  end
  
  def distance(other_pp)
    (other_pp.players - players).size
  end
    
  def ==(other_pp)
    other_pp.id == id
  end
  
  def eql?(other_pp)
    other_pp == self
  end
  
  def hash
    id.hash
  end
  
  def +(other_possible_players)
    PossiblePlayers.new(players + other_possible_players.players)
  end
  
  def -(players_to_remove)
    PossiblePlayers.new(players - Array(players_to_remove))
  end
  
  def points(metric = :points)
    @points[metric] ||= players.map(&metric).map(&:to_f).sum.t
  end
  
  def projections
    @projections ||= [
       players.map(&:projection_low).map(&:to_f).sum.t,
       players.map(&:projection_consensus).map(&:to_f).sum.t,
       players.map(&:projection_high).map(&:to_f).sum.t
    ]
  end
  
  def bfb(metric = :bfb_consensus)
    @bfb_consensus ||= players.map(&metric).map(&:to_f).sum
  end
  
  def value
    @value ||= players.map(&:value_for_money).map(&:to_f).sum
  end
  
  def full?
    players.size == POSITIONS
  end
  
  def cost
    @cost ||= players.map(&:s).map(&:to_f).sum
  end
  
  def remaining_budget
    BUDGET - cost
  end
  
  def average_allowable_salary
    remaining_budget.to_f / (POSITIONS - players.size)
  end
  
  def not_too_expensive?
    average_allowable_salary > 2500
  end
  
  def under_budget?
    remaining_budget >= 0
  end
  
  def cap_space_value
    points * Math.log10(remaining_budget)
  end
end

class Player
  FLEX_POSITIONS = Set.new(["RB", "WR", "TE"])
  
  attr_reader :struct
  def initialize(hash)

    hash = hash.dup
    
    hash['ppg'] = hash['ppg'].to_f if hash['ppg']
    if hash['salary']
      hash['salary'] = hash['salary'].to_i 
      hash['s'] = hash['salary']
    else
      hash['salary'] = hash['s']
    end
    
    if pro = hash['projections']    
      hash['projection_low'] = pro['conservative']['projectedPoints']
      hash['projection_consensus'] = pro['consensus']['projectedPoints']
      hash['projection_high'] = pro['aggressive']['projectedPoints']
      hash['bfb_low'] = pro['conservative']['bangForYourBuckScore']
      hash['bfb_consensus'] = pro['consensus']['bangForYourBuckScore']
      hash['bfb_high'] = pro['aggressive']['bangForYourBuckScore']
      hash.delete('projections')
    else
      hash['projection_low'] ||= -1
      hash['projection_consensus'] ||= -1
      hash['projection_high'] ||= -1
      hash['bfb_low'] ||= 1000000000
      hash['bfb_consensus'] ||= 1000000000
      hash['bfb_high'] ||= 1000000000
    end
    @struct = OpenStruct.new(hash)
  end
  
  def id
    ffn? ? playerId : pid
  end
  
  def flex_position?
    FLEX_POSITIONS.include?(pn)
  end
  
  def name
    (self[:name] || "#{fn} #{ln}").strip
  end
  
  def value_for_money
    # struct.ppg / struct.s * struct.ppg * 100 # Points divided by cost times points
    #points / salary * points * 100.0
    @value ||= begin
      if projection_consensus == -1
        points.to_f / salary.to_f * points * 100.0
      else
        total = projections.sum.to_f
        total / salary.to_f * total * 100.0
      end
    end
  end
  
  def ffn?
    projection_consensus != -1
  end
  
  def dk?
    !ffn?
  end
  
  def points
    # ppg
    if projection_consensus == -1
      ppg
    else
      projection_consensus
    end
  end
  
  def projections
    [projection_low, projection_consensus, projection_high]
  end
  
  def pn
    position
  end
  
  def position
    self[:position] || self[:pn]
  end
  
  def pid
    self[:playerId] || self[:pid]
  end
  
  def cost
    s
  end
  
  def s
    salary.to_i
  end
  
  def value
    value_for_money
  end
  
  def method_missing(method, *args, &block)
    if struct.respond_to?(method)
      struct.send(method, *args, &block)
    else
      super
    end
  end
end

module Enumerable
  def rsort_by(&block)
    sort_by {|e| -block.call(e)}
  end
end

class Array
  def take_percent(percent)
    self[0..(size*(percent/100.0)).to_i]
  end

  def mean
    sum / size.to_f
  end
  
  def sum
    inject(&:+)
  end

  def sample_variance
    m = mean
    s = inject(0){|accum, i| accum +(i-m)**2 }
    s/(size - 1).to_f
  end

  def standard_deviation
    Math.sqrt(sample_variance)
  end
end

class Numeric
  def t
    Integer(self * 100) / 100.0
  end
end

if __FILE__ == $0
  def dk_to_csv(path)
    json = JSON.parse(IO.read(path))
    players = json['playerList'].map {|pl| Player.new(pl)}
    
    puts "name,salary,points,position,id"
    
    players.each do |player|
      row = [:name, :salary, :points, :position, :id].map {|prop| player.send(prop)}.join(",")
      puts row
    end
  end
  
  def ffn_to_csv(path)
    json = JSON.parse(IO.read(path))
    players = json['players'].map {|pl| Player.new(pl)}
    
    puts "name,salary,points,position,id"
    
    players.each do |player|
      row = [:name, :salary, :points, :position, :id].map {|prop| player.send(prop)}.join(",")
      puts row
    end
  end
  
  path = ARGV[0] || abort("Error: Provide path to json")
  
  dk_to_csv(path)
end


#### strategies:
# players on teams picked to win
# budget/roster spots budget for each player
# winning qb with best receiver on their team
# max out studs at QB/RB1/WR1
# players on teams picked to lose
# players going against the bottom 50% of defenses
# players on teams in the top 50% of defenses
# max out budget on WR
# max out budget on RB
# max out budget on TE
# max out budget on QB
# roster that is projected to exceed 50th percentile weekly score in 50/50 contests
# highest salary to lowest % drafts (i.e. sleepers) can you get % drafted before the games?

# => {"pid"=>456614, "pcode"=>24793, "tsid"=>3149082, "fn"=>"Julio", "ln"=>"Jones", "fnu"=>"Julio", "lnu"=>"Jones", "pn"=>"WR", "tid"=>323, "htid"=>323, "atid"=>363, "htabbr"=>"Atl", "atabbr"=>"Was", "posid"=>3, "slo"=>nil, "IsDisabledFromDrafting"=>false, "ExceptionalMessages"=>[], "s"=>9200, "ppg"=>"29.7", "or"=>21, "swp"=>true, "pp"=>0, "i"=>"", "news"=>2}

# "i" is playing status, e.g. "P", "Q" or blank
