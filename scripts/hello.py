"""Our first Ged script."""

def hello(name):
    print 'Hello %s!' % name


def main(argv):
    name = argv[0]
    hello(name)
