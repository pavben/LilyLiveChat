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
	return env.Command(name, [
		Glob('Liberty/' + name + '/*.hs'),
		Glob('Liberty/Common/*.hs'),
		Glob('Liberty/Common/Messages/*.hs')
	],
	'cd ' + env.Dir('.').abspath + ' && ' +
	'export HOME=/home/`whoami` && ' +
	'ghc Liberty/' + name + '/Main.hs -o ' + name
)

Export('buildBasicHaskellServer')

sconscripts = [
	'servers/ChatServer.scons',
	'servers/ChatStatusService.scons',
	'servers/MainWebsite.scons',
	'servers/SiteDataService.scons',
	'servers/SiteLocatorService.scons',
	'servers/WebChatInterface.scons'
]

for sconscript in sconscripts:
	SConscript(sconscript, variant_dir=variantDir, duplicate=1)

