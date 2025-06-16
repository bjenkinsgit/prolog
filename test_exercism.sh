#!/usr/bin/env bash
curl -ILv --max-redirs 20 \
     -A "Mozilla/5.0" \
     -H "Accept-Language: en-US,en;q=0.9" \
     https://exercism.org 2>&1 |
grep -E '^(< HTTP/|< Location:|> GET|\*  Issue)'
