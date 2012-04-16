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

	if ($line =~ m/^Server to/) {
		$ignoreMode = 1;
		next;
	}

	if ($line =~ m/to Server$/) {
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

		my $out = "jsonToMP " . $msgType . " [";

		my @params = split(/\s?,\s?/, $2);

		my @hparams1;
		my @hparams2;

		for (@params) {
			if ($_ =~ m/^i:(.*)/) {
				push (@hparams1, "J.Number (DAN.I " . $1 . ")");
				push (@hparams2, "fromInteger " . $1 . " :: Int");
			} else {
				push (@hparams1, "J.String " . $_);
				push (@hparams2, $_);
			}
		};

		$out .= join(', ', @hparams1);

		$out .= "] =\n  createMessage " . $msgType . " (";

		$out .= join(', ', @hparams2);

		$out .= ")";

		print $out, "\n\n";
	}
}

