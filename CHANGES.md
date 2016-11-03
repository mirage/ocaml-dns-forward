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
