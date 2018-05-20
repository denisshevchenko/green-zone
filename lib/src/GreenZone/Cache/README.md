## Cache

Loops work independently in their loops, but they need some form of interconnection.
This is the purpose of the cache.

'Cache' is a data structure which is shared among loops. Any loop can atomically
read value(s) from the cache and/or write value(s) to the cache. It's implemented
using STM (Software Transactional Memory) mechanism.
