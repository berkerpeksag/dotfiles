#!/usr/bin/env python
# coding: utf-8

from datetime import datetime, date, timedelta
from sys import argv

import requests

BASE_URL = 'http://api.twitter.com/1/'
DATE_RANGE = date.today() - timedelta(days=30)


def convert_date(date_string):
    return datetime.strptime(date_string, '%a %b %d %H:%M:%S +0000 %Y').date()


def request(url):
    r = requests.get(BASE_URL + url)
    if r.status_code is requests.codes.ok:
        return r
    r.raise_for_status()


def get_following_ids(screen_name):
    r = request('friends/ids.json?screen_name={0:s}'.format(screen_name))
    return r.json['ids']


def get_user_tweets(user_id):
    r = request('users/show.json?user_id={0:d}'.format(user_id))
    if 'status' in r.json:
        result = (r.json['screen_name'],
                  convert_date(r.json['status']['created_at']))
    else:
        result = (r.json['screen_name'], None)
    return result


def main(screen_name):
    following_ids = get_following_ids(screen_name)

    for user_id in following_ids:
        screen_name, last_tweet_date = get_user_tweets(user_id)
        print 'Checking {0:s}...'.format(screen_name)
        if isinstance(last_tweet_date, date):
            if last_tweet_date <= DATE_RANGE:
                print screen_name, last_tweet_date
        else:
            print '{0:s}\'s account is protected.'.format(screen_name)


def usage():
    exit('Usage: python {0} <TWITTER_SCREEN_NAME>'.format(__file__))

if __name__ == '__main__':
    if len(argv) is not 2 or not isinstance(argv[1], basestring):
        usage()
    main(argv[1])
