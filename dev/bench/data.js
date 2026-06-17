window.BENCHMARK_DATA = {
  "lastUpdate": 1781724617108,
  "repoUrl": "https://github.com/graceto2/jsip-exchange",
  "entries": {
    "Order book benchmark": [
      {
        "commit": {
          "author": {
            "email": "129772334+graceto2@users.noreply.github.com",
            "name": "Grace To",
            "username": "graceto2"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "fb805aace7f4c7d6ad49112894ed4e9a13382e47",
          "message": "Merge branch 'jane-street-immersion-program:main' into main",
          "timestamp": "2026-06-17T15:26:46-04:00",
          "tree_id": "b105f708f1d0a3bfac0fc8f703926fc5cb5958f3",
          "url": "https://github.com/graceto2/jsip-exchange/commit/fb805aace7f4c7d6ad49112894ed4e9a13382e47"
        },
        "date": 1781724616836,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "find_match (n=10)",
            "value": 24.822042750345894,
            "unit": "ns"
          },
          {
            "name": "find_match (n=50)",
            "value": 24.50311646322808,
            "unit": "ns"
          },
          {
            "name": "find_match (n=100)",
            "value": 24.638276568471444,
            "unit": "ns"
          },
          {
            "name": "find_match (n=500)",
            "value": 24.844445441408944,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=10)",
            "value": 115.73443292911848,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=50)",
            "value": 525.4802370126022,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=100)",
            "value": 1124.0783976291405,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=500)",
            "value": 5646.392061474881,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=10)",
            "value": 217.4575320879194,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=50)",
            "value": 1121.068020879949,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=100)",
            "value": 2191.2611717450873,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=500)",
            "value": 10242.802290577541,
            "unit": "ns"
          },
          {
            "name": "add+remove (n=100)",
            "value": 1659.6393834550554,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=10)",
            "value": 1276.217662233637,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=50)",
            "value": 4934.008104568581,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=100)",
            "value": 9835.201076513145,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=500)",
            "value": 46860.99791758218,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=10)",
            "value": 626.5096674436942,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=50)",
            "value": 2833.668930148389,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=100)",
            "value": 5565.595918808903,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=500)",
            "value": 27419.22222882159,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_10_levels",
            "value": 5681.961387155346,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_50_levels",
            "value": 89519.00989274326,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_100_levels",
            "value": 311760.9053013064,
            "unit": "ns"
          },
          {
            "name": "find_match_alloc (n=100)",
            "value": 24.652126504631077,
            "unit": "ns"
          }
        ]
      }
    ]
  }
}