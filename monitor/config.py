__all__ = ('settings',)

settings = {}

try:
    from config_local import settings
except ImportError:
    raise NotImplemented('/etc/autopour/monitor.conf')
