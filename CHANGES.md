### 0.8.4 (2017-03-28)

- if we fail to resolver a name, timeout rather than sending NXDomain
- switch to jbuilder
- split into 2 opam packages: `dns-forward` and `dns-forward-lwt-unix`

### 0.8.3 (2017-03-12)

- if we fail to resolve a name, return NXDomain rather than simply timing out
- avoid logging per-request if no servers are configured at all

### 0.8.2 (2017-03-10)

- add global configuration option `assume-offline-after-drops` which tells the resolver to consider an upstream offline if it fails to respond to this number of queries in a row
- when a server is offline we continue to send queries to it, but we don't wait for responses. This prevents a bad server making us wait until timeout on every request.
- we use `Lwt.nchoose_split` to wait for equal-priority results at once: this avoids waiting for requests in arbitrary list order (previously we used `Lwt.fold_left_s`)
- we resend requests every 1s even if the timeout is longer

### 0.8.1 (2017-02-06)

- always set the Recursion Available bit from cached responses

### 0.8.0 (2017-01-11)

- send all requests in parallel, process results in order
- don't cancel listening threads when the first response arrives, so
  we can cache all server responses
- servers with zones are now included in general searches
- prioritise successful responses over failures

### 0.7.2 (2016-11-18)

- Fix build with OCaml 4.04

### 0.7.1 (2016-11-18)

- Remove some spammy debug messages

### 0.7.0 (2016-11-15)

- Fix race where a timeout could mark an id as free and an
  old answer to the wrong question could be confused with
  the answer we're expecting
- Minimise amount of transaction id re-use

### 0.6.0 (2016-11-14)

- If a single server fails, don't cancel all other resolution
  attempts.

### 0.5.1 (2016-11-11)

- Silence some of the spammy logging

### 0.5.0 (2016-11-11)

- Cache and local names callback: return packets of type
  "response" rather than "request"

### 0.4.0 (2016-11-03)

- Cache results for up to the TTL

### 0.3.0 (2016-11-03)

- Require lwt >= 2.6.0 for Lwt_results
- Remove per-request timeout value: clients should cancel thread
- Add per-server timeout value
- Add notion of server ordering

### 0.2.0 (2016-10-26)

- Remove `getclientname` from `Flow.Client`
- Make the addresses in the `message_cb` functions optional

### 0.1.0 (2016-10-25)

- Initial version
