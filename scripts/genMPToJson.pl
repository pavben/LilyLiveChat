open(MYINPUTFILE, "<protocol");

my $ignoreMode = 1;

while(<MYINPUTFILE>)
{
	my($line) = $_;

	chomp($line);
	
	if ($line =~ m/^[-=]/) {
		next;
	}

	if ($line =~ m/^$/) {
		next;
	}

	if ($line =~ m/to Server$/) {
		$ignoreMode = 1;
		next;
	}

	if ($line =~ m/^Server to/) {
		#print "ignore mode OFF\n";
		$ignoreMode = 0;
		next;
	}

	if ($ignoreMode) {
		#print "ignoring line: ", $line, "\n";
		next;
	}
	#print "$line\n";

	if ($line =~ m/^\d+\s*-\s*(\w+)(?:\s*\[(.*?)\])?/) {
		my $msgType = $1;
		#print "Match: [", $msgType, "] [", $2, "]\n";

		my $out = "messageToJson " . $msgType . " encodedParams =";

		my @params = split(/\s?,\s?/, $2);

		my @hparams1;
		my @hparams2;

		for (@params) {
			if ($_ =~ m/^i:(.*)/) {
				push (@hparams1, $1 . " :: Int");
				push (@hparams2, $1);
			} elsif ($_ =~ m/^b:(.*)/) {
				push (@hparams1, $1 . " :: Bool");
				push (@hparams2, $1);
			} else {
				push (@hparams1, $_ . " :: Text");
				push (@hparams2, $_);
			}
		};

		my @hparams3;
		push (@hparams3, "J.toJSON (messageTypeToId " . $msgType . ")");
		for (@hparams2) {
			push (@hparams3, "J.toJSON " . $_);
		}

		$out .= "\n  unpackAndHandle encodedParams \$ \\(";

		$out .= join(', ', @hparams1);

		$out .= ") -> [";

		$out .= join(', ', @hparams3);

		$out .= "]";

		print $out, "\n\n";
	}
}

