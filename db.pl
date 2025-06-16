tweep(jill).
tweep(eric).
tweep(jose).
tweep(joe).
tweep(anna).

tweet(gossip).
tweet(policy).
tweet(declaration).
tweet(greeting).
tweet(screen).

tweets(jill, greeting).
tweets(anna, gossip).

follows(eric, jill).
follows(joe, anna).
follows(eric, joe).
follows(jill, joe).

receives(Tweep, Tweet) :-
  tweets(Someone, Tweet),
  follows(Tweep, Someone).
