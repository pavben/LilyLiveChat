import os

env = Environment()

variantDir = 'build';

AddOption(
	'--release',
	action='store_true',
	help='release build',
	default=False)

commonCompileFlags = [
	'-cpp'
]

if GetOption('release'):
	commonCompileFlags += []
	variantDir = os.path.join(variantDir, 'release')
else:
	commonCompileFlags += ['-DDEBUG']
	variantDir = os.path.join(variantDir, 'debug')

def buildBasicHaskellServer(name):
	env.Command(name, [
			Glob('Liberty/' + name + '/*.hs'),
			Glob('Liberty/Common/*.hs'),
			Glob('Liberty/Common/Messages/*.hs')
		],
		'cd ' + env.Dir('.').abspath + ' && ' +
		'export HOME=/home/`whoami` && ' +
		'ghc Liberty/' + name + '/Main.hs -o ' + name
	)
	env.Install('../output/', name)

Export('env')
Export('buildBasicHaskellServer')

sconscripts = [
	'mainweb.scons',
	'servers/ChatServer.scons',
	'servers/ChatStatusService.scons',
	'servers/MainWebsite.scons',
	'servers/SiteDataService.scons',
	'servers/SiteLocatorService.scons',
	'servers/WebChatInterface.scons'
]

SConscript('mainweb.scons', variant_dir=variantDir, duplicate=1)
SConscript('chatweb.scons', variant_dir=variantDir, duplicate=1)

SConscript('servers/ChatServer.scons', variant_dir=variantDir + '/servers', duplicate=1)
SConscript('servers/ChatStatusService.scons', variant_dir=variantDir + '/servers', duplicate=1)
SConscript('servers/MainWebsite.scons', variant_dir=variantDir + '/servers', duplicate=1)
SConscript('servers/SiteDataService.scons', variant_dir=variantDir + '/servers', duplicate=1)
SConscript('servers/SiteLocatorService.scons', variant_dir=variantDir + '/servers', duplicate=1)
SConscript('servers/WebChatInterface.scons', variant_dir=variantDir + '/servers', duplicate=1)

