require 'benchmark'

N = 100

%w(sml js).each do |ext|
  bm = Benchmark.measure do
    N.times { `cat perf/benchmark.json | perf/squish-#{ext} > /dev/null` }
  end
  puts ext, bm
end