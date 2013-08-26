
import boto, sys, re, json
from boto.dynamodb.condition import GT

tablename = sys.argv[1]
hash_key = sys.argv[2]
tweetid = sys.argv[3]

# aws keys in /etc/boto.cfg, no need to put them here...
conn = boto.connect_dynamodb()
table = conn.get_table(tablename)
item = table.query(hash_key=hash_key, 
                   range_key_condition=GT(int(tweetid)),
                   attributes_to_get=['search_term','tweetid', 'author', 'text', 
                                      'timestamp_pretty'])

tweets = '{"tweets":['
for i in item:
    #print json.dumps(convert(i))
    tweets = tweets + json.dumps(i)
tweets = re.sub("}\s*{", "},{", tweets)
tweets = tweets + ']}'
print(tweets)

