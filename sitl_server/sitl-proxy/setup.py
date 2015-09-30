
from distutils.core import setup

setup(
    name             = 'sitl_proxy',
    version          = '1.0.0',
    description      = '''MAVProxy module for proxying information to a haskell SITL server''',
    author           = 'Galois, Inc.',
    author_email     = 'trevor@galois.com',
    packages         = ['sitl_proxy'],
    install_requires = ['pymavlink', 'MAVProxy']
    )
