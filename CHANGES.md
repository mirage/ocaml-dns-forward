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
