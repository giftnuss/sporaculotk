#
# vim:ts=2:sw=2
# Package: Orac::Shell::Do
# contains the database interaction commands.
#

package Shell::Do;

sub do_prepare {
	my ($self, $statement) = @_;
	my $sth;

	unless($statement) {
		$self->{c}->{status} = -1;
		$self->{c}->{msg} =  "Preparing a blank statement ... ? (doesn't work)";
		return undef;
	}

	local $SIG{__WARN__} = 'DEFAULT';

	eval {
		$sth = $self->{dbh}->prepare($statement);
	};
	if ($@ or $DBI::err) {
		my $err = $@;
			$self->{c}->{status}  = $DBI::err;
			# Use an error status, if eval error.
			$self->{c}->{status}  = -1 unless $DBI::err;
			$self->{c}->{msg} =  $err . $DBI::errstr;
			return $self->{c}->{status};
	}
	return $sth;
}

sub sth_go {
	my ($self, $sth, $execute) = @_;
	my $rv;

	local $SIG{__WARN__} = 'DEFAULT';

	if ($execute or !$sth->{Active}) {
		my @params;
		my $params = $sth->{NUM_OF_PARAMS} || 0;
		print "Statement has $params parameters:\n" if $params;
	  @params = $self->get_params($params) if $params;
			eval {
				$rv = $sth->execute(@params);
			};
			if ($@ or $DBI::err) {
				local $^W=0;
				my $err = $@;
				# Pick up the DBI error first.
				$self->{c}->{status}  = $DBI::err;
				# Use an error status, if eval error.
				$self->{c}->{status}  = -1 unless $DBI::err;
				$self->{c}->{msg} = "$err" . $DBI::errstr;
				return $self->{c}->{status};
			}
			return $rv if !defined($rv); 
			$self->{c}->{display} = 1;
	}
	
	if (!$sth->{'NUM_OF_FIELDS'}) { # not a select statement
		local $^W=0;
		$rv = "undefined number of" unless defined $rv;
		$rv = "unknown number of"   if $rv == -1;
		$self->{c}->{msg} = "[$rv row" . ($rv==1 ? "" : "s") . " affected]"; #"
		$self->{c}->{display} = 0;
		$self->{c}->{status} = 0;
	}

	$rv;
}

sub do_execute {
	my $self = shift;
	my $sth  = shift;

	local ($SIG{__WARN__}) = 'IGNORE';

$sth->execute(@_);
}
sub do_finish {
	my $self = shift;
}
sub do_fetch {
	my $self = shift;
}
sub do_commit {
	my $self = shift;
}
sub do_rollback {
	my $self = shift;
}
sub do_do {
	my $self = shift;
$self->{dbh}->do( @_ );
}

sub get_params {
	my ($self, $num ) = @_;

	my @val;
	my $d = $self->{dbiwd}->Dialog( -title => 'Enter Parameters',
	);

	for my $x ( 1 .. $num ) {
		my $f = $d->Frame()->pack( -side => 'top' );
		my $l = $f->Label( -text => "Param: $x", 
			-relief => 'groove',
			-width => 20 );
		my $e = $f->Entry( -width => 20, -textvariable => \$val[$x - 1] );
		$l->pack( -side => 'left' );
		$e->pack( -side => 'right' );
	}

	$d->Show;

	return @val;
}

1;
__END__
