# "Release" Changelog

This directory tree exists for convenience.

I took a deviation from my original plans. I would consider each snapshot a completed version of the project. Format is nn.xxxxxxx, where `n` exists for sequencing and `x` is the commit hash.

My original goal was to provide a complete UUID solution. I noted that version 2 UUIDs were not typically generated in implementations I saw and wanted to include them. After implementing them in a simple manner it became apparent that the management of them would be rather complex and not my use case. This is because the `ClockSeq` field wraps after 63 calls within a ~430 second period for a given user / group / org value in `Time-Low`.

Might build on in the future but will finish main branch simplifying the API, allowing batch requests and increasing performance. 
