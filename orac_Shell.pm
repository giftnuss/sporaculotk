#
# vim:set ts=2:sw=2:ai:aw:
################################################################################
#
# Orac DBI Visual Shell.
# Versions 1.0.9a
#
# Copyright (c) 1998,1999 Andy Duncan, Thomas A. Lowery
#
# You may distribute under the terms of either the GNU General Public License
# or the Artistic License,as specified in the Perl README file,with the
# exception that it cannot be placed on a CD-ROM or similar media for commercial
# distribution without the prior approval of the author.
#
# This code is provided with no warranty of any kind,and is used entirely at
# your own risk. This code was written by the author as a private individual,
# and is in no way endorsed or warrantied.
#
# Support questions and suggestions can be directed to 
# Download from CPAN/authors/id/A/AN/ANDYDUNC
################################################################################
#


package orac_Shell;
@ISA = qw{
	Shell::Do
	Shell::File
	Shell::Format
	Shell::Mark
	Shell::Menu
	Shell::Meta
	Shell::Properties
	};

use Exporter;

use Tk '800.014';
use Tk::Pretty;
use Tk::Dialog;
use Tk::Adjuster;

use FindBin;
use lib $FindBin::RealBin;

use Shell::Do;
use Shell::Format;

use Data::Dumper;

use strict;

my $VERSION;
$VERSION = $VERSION = qq{1.0.9a};

my ($sql_txt, $sql_entry_txt);
my ($rslt_txt, $rslt_entry_txt);
my ($ind_txt, $mv_ind_rslt);
my (@ind_txt, @mv_ind_rslt, $ind_tbl);
my (@sql_txt, @sql_entry_txt);

my ($opt_row_dis_c, $opt_dis_grid);
my ($entry_txt, $entry_tbl, $entry_frm);
my (@entry_txt);
my %color_ball;
my ($dbiwd, $dbistatus, $auto_ball, $chng_ball, $button_exe, $autoexec);
my ($idxMark, $begLn);
my $exeStatement;
my %rmarkers;

my $c;  # $c is the reference to the "current" SQL statement.
my $prop; # properties for the current user session.
my $menu_file_prop; # Store the menu file property.
my $meta; # Meta data from DBI and current database.
my %buttons; # Maintain a list of buttons.  Only those requiring state changes.
my $auto_commit; # Track the current state of auto commit.

sub new
{
   print STDERR "orac_Shell::new\n" if ( $main::debug > 0 );

	my $proto = shift; 
	my $class = ref($proto) || $proto;
	my $marks = Shell::Mark->new;
	my $current = {
		current   => undef,
		status    => $dbistatus,
		beg       => $begLn,
		end       => undef,
		cmark     => undef,
		display   => undef,
		stat_num  => undef,
		is_marked => undef,
		msg       => undef,
	};

	my $self  = {
		c => $current,
		debug => $main::debug,
		display_rows => 1,
		marks => $marks,
		menus => {
			file => undef,
			edit => undef,
			meta => undef,
			options => undef,
			help => undef,
			order => [qw/Help File Edit Meta Options/],
		},
		n =>  undef,
		options => {
			debug => $main::debug,
			autoexec => 1,
			display => q{neat},
			font => undef,
		},
		rv         => undef,
		status  => $dbistatus,
	};

   bless($self, $class);

   print Dumper($self) if ( $self->debug > 0 );

   # save off args...
   # or other encapsulated values, these do NOT inherit!

   $self->{mw} = $_[0];
   $self->{dbh} = $_[1];

   print STDERR "orac_Shell,   new mw  >$self->{mw}<\n" 
      if ( $self->debug > 0 );

   print STDERR "orac_Shell,   new dbh >$self->{dbh}<\n" 
     if ( $self->debug > 0 );

   $color_ball{green} = $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/grn_ball.gif" );
   $color_ball{red}   = $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/red_ball.gif" );
   $color_ball{yellow}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/yel_ball.gif" );

	 # Checkmark for results window
	 $color_ball{checkmark}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/tick.gif");

   $color_ball{exec}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/exec_tick.gif");
   $color_ball{execall}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/exec_all.gif");
   $color_ball{clear}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/eraser.gif");

   $color_ball{commit}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/th_up.gif");

   $color_ball{rollback}= $self->{mw}->Photo( 
	 	-file => "$FindBin::RealBin/img/th_dn.gif");

	$c = $self->{c};

	#
	# Define the methods to handle the properties.
	#
	$prop = Shell::Properties->new( $self );
	$meta = Shell::Meta->new( $self );
	return $self;
}

# Create the top level for Orac DBI Shell.



sub dbish_open {
   my $self = shift;
   my $mw = $self->{mw};

   #
   # Determine if the dbish window is defined.  If it isn't, define the
   # window.
   #

   if (defined($self->{dbiwd})) {

      print "deiconifying\n";

      $dbiwd->deiconify();
      $dbiwd->raise();

   } else { 
      
      # Create a Toplevel window under the current main.

      $dbiwd = $self->{mw}->Toplevel();

      $dbiwd->title( "Orac-DBA SQL Shell" );

      # Create status label

      my $sf = $dbiwd->Frame( -relief => 'groove',
                              -bd => 2 
                            )->pack( -side => 'bottom', -fill => 'x' );

      # This is the status bar.

      $auto_ball = $sf->Label(-image=> $color_ball{green}, 
                              -borderwidth=> 2,
                              -relief=> 'flat'
                             )->pack(  -side=> 'right', -anchor=>'w');

			$auto_ball->bind( q{<Enter>}, 
				sub {
					my $msg = qq{Auto Commit is };
					$msg .= qq{OFF} unless $auto_commit;
					$msg .= qq{ON}  if $auto_commit;
					$msg .= qq{.  Click to change.};
					$dbistatus = $msg;
			} );
			$auto_ball->bind('<Leave>', sub { $dbistatus = ""; } );
			$auto_ball->bind(q{<Button-1>},
				sub { $self->auto_commit(); } );

      #$chng_ball = $sf->Label(-image=> $color_ball{green}}, 
                              #-borderwidth=> 2,
                              #-relief=> q{flat}
                             #)->pack(  -side=> q{right}, -anchor=>'w');

      #$self->bind_message( $chng_ball, q{Changes committed to database?} );

      $sf->Label( -textvariable => \$dbistatus )->pack(-side => 'left');

      my @menus;

      # Create the menu bar with entries.
      $self->menu_bar();

      # Create the menu button with entries.
      $self->menu_button();

      # Create Text widget for results display.
      $dbistatus = "Creating Text results widget";

      $self->new_entry( $_ );

      $dbiwd->{text} = $rslt_txt;

      # Tie the windows to the file types:
      tie (*ENTRY_TXT, 'Tk::Text', $entry_txt);
      tie (*RSLT_TXT,  'Tk::Text', $rslt_txt);

      $self->{dbiwd} = $dbiwd;

      # Finallly, iconize window

      main::iconize($dbiwd);
   }

   # If the auto commit is on, set the ball to green, else red.
   # This is adjusted each time the window is entered. 

   if($auto_commit = $self->{dbh}->{AutoCommit}) {
      $auto_ball->configure( -image => $color_ball{green} );
   } else {
      $auto_ball->configure( -image => $color_ball{red} );
   }

   # Make the Main Window and icon.
        # Disable button, calling orac_Shell

   $self->{mw}->iconify();

   $dbistatus = "Window for Tk created.";

   # Last thing is focus on the entry box.
   $entry_txt->focusForce();
   $rslt_txt->configure( -state => 'disabled' );
	$c = $self->{c};

}

my ($curx);
sub where_am_i {
   my ($self, $x, $y, $txt ) = @_;
   $curx = $x;
   $entry_txt->focus();

   print STDERR "Move to : $x\n"
      if ( $self->debug > 0 );
};

sub new_entry {
	my ($self, $x) = @_;
	my $main_frame = $dbiwd->Frame( -relief => 'groove',
                                    )->pack(-fill=>'both', 
                                            -expand => 1,
                                            -side => 'top' 
                                           );

	$rslt_txt = $main_frame->Scrolled( "Text", 
                                         -relief => 'groove',
                                         -width => 78, 
                                         -height => 10,
                                         -cursor=>undef,
                                         -foreground=>$main::fc,
                                         -background=>$main::bc,
                                         -font=>$main::font{name},
                                         -wrap => "none",
                                         -takefocus => 0,
                                         -setgrid => 1,
     
                                       );


      $entry_frm = $main_frame->Frame( -relief => 'groove',
                                     )->pack(-fill=>'both', 
                                             -expand => 1,
                                             -side => 'top' 
                                            );


      # Create Text widget for command entry
      $dbistatus = "Creating Text entry widget";

      my $adjuster = $main_frame->Adjuster();

      $adjuster->packAfter(  $entry_frm, 
                             -side => 'top',
                          );

	my $s = $entry_frm->Scrolled( 'Text',
		-scrollbars => 'wo',
		-height => 12,
        -setgrid => 1,
	)->pack(-expand =>1, -fill=> "both");

   my $ysb = $s->Subwidget( "yscrollbar" );

   $ind_txt = $s->Text( 
	 	-width => 0,
		-state => 'normal',
		-borderwidth => 0,
		); 
   my ($this,$last);
   $this = $last = 0;
   my $ss = sub {
      $s->Subwidget( "yscrollbar" )->set(@_);
      $this = $s->Subwidget( "yscrollbar" )->get();
      if ($this < $last) {
         $ind_txt->yview("scroll", -1, "units" );
      } else {
         $ind_txt->yview("scroll", 1, "units" );
      }

      print STDERR "This $this Last $last\n"
         if ( $self->debug > 0 );

      $last = $this;

      print STDERR "entry index current: " , 
                   $entry_txt->index( 'current' ), "\n"
         if ( $self->debug > 0 );
   };


   $entry_txt = $s->Text( # 'Text',
      -relief => 'groove',
      -width => 74,
      # -height => 3,
      -cursor=>undef,
      -foreground=>$main::fc,
      -background=>$main::ec,
      -yscrollcommand => $ss,
			-borderwidth => 0,
			-setgrid => 1,
   );


	my $sc = sub {
      $entry_txt->yview(@_);
      #$ind_txt->yview(@_);
	};

	$ysb->configure( -command => $sc );


	#$ind_txt->pack( 
		#-side => 'left',
		#-pady => 1,
		#-expand => 0,
		#-fill => 'both',
	#);
	$entry_txt->pack(
		-side => 'left',
		-pady => 1,
		-expand => 1,
		-fill => 'both',
	);
	$rslt_txt->pack( -expand=>1, -fill=>'both', -side => 'top' );
	$ind_txt->configure( -state => 'disabled', );

	# Pick up the Return key ... see return_press.
	$entry_txt->bind( "<Return>", sub { $self->return_press() } );
	$entry_txt->tagConfigure( "Exec",  -foreground => "green" );
	$entry_txt->tagConfigure( "Error",  -foreground => "red" );

	# Tags for the results window.
	$rslt_txt->tagConfigure( "Bold", -background => "white" );

}

#
#  Allows the user to change auto commit.
#
sub auto_commit {
	my ($self) = @_;
	if($self->{dbh}->{AutoCommit}) {
		# AutoCommit is on, turn off, enable buttons.
		$self->{dbh}->{AutoCommit} = 0;
		$auto_ball->configure( -image => $color_ball{red} );
		$buttons{commit}->configure( -state => q{normal} );
		$buttons{rollback}->configure( -state => q{normal} );
		$auto_commit = 0;
	} else {
		# AutoCommit is off, turn on, disable buttons.
		$auto_ball->configure( -image => $color_ball{green} );
		$buttons{commit}->configure( -state => q{disabled} );
		$buttons{rollback}->configure( -state => q{disabled} );
		$self->{dbh}->{AutoCommit} = 1;
		$auto_commit = 1;
	}
}

#
# Commit: Only works if auto commit is off.
#
sub commit {
	my $self = shift;
	$self->{dbh}->commit;
	$rslt_txt->configure( -state => q{normal} );
	print RSLT_TXT qq{\nCommitted!\n};
	$rslt_txt->configure( -state => q{disabled} );
}

#
# Rollback: Only works if auto commit is off.
#
sub rollback {
	my $self = shift;
	$self->{dbh}->rollback;
	$rslt_txt->configure( -state => q{normal} );
	print RSLT_TXT qq{\nRolled Back!\n};
	$rslt_txt->configure( -state => q{disabled} );
}

#
# Because I really dislike have to move the mouse up to the execute
# button, if the only character on a line is /, execute the above
# statement.
#
sub return_press {
   my $self = shift;

   # Determine where the cursor is.
   my $ind = $entry_txt->index( 'insert - 1 lines lineend' );

   # Grab the last line of text.
   my $txt = $entry_txt->get( "$ind - 1 chars" , "$ind" );
   chomp $txt;

	# The previously entered line is only a /, exeucte.
	if ($txt =~ m:[/;]$: and $autoexec) {

		print STDERR "An execute command ...\n" if ( $self->debug > 0 );
		$self->doit( $ind );

	} 
}

sub check_ind_txt {
   my ($self, $pl) = @_;

   print $ind_txt->index('end'), "\n"
      if ( $self->debug > 0 );

   if ($ind_txt->compare( 'end', "<=", "$pl.0" ) ) {
      $ind_txt->insert( 'end', "\n" );
   }
}

#
# Tag Statement Errored: The statement failed to execute.
#

sub tag_statement_errored {
	my ($self, $beg, $end) = @_;

	$beg = $c->{beg} unless $beg;
	$end = $c->{end} unless $end;

	$entry_txt->tagAdd( 'Error', $beg, $end );
	$self->{marks}->remove($c->{stat_num});
}

sub untag_statement_errored {
	my ($self, $beg, $end) = @_;

	$beg = $c->{beg} unless $beg;
	$end = $c->{end} unless $end;

	$entry_txt->tagRemove( 'Error', $beg, $end );
}

#
# Create the results button next to the statement
# executed.  The results button is only created, if
# one does not currently exist.
#

sub create_results_btn {
	my ($self, $bl) = @_;
	$bl = $c->{stat_num} unless $bl;
	if (!$c->{is_marked}) {
		return $self->set_results_btn($bl);
	}
	return 0;
}
# 
# Determine where the markers go
#


sub set_results_btn {
	my ($self, $c, $inx ) = @_;

	$c = $c->{stat_num} unless $c;
	print STDERR "Statement number: $c\n"
         if ( $self->debug > 0 );

	# Enable the ind widget to accept the line number.
	$ind_txt->configure( -state => 'normal' );

	 # Round off the current
   my $pl = int( 
	 	$entry_txt->index( $self->{marks}->get_mark_beg($c)) + .99 );

   print STDERR "Statement num: $c Begin: $pl\n" if ( $self->debug > 0 );

   $self->check_ind_txt($pl);

   # Add just a line place for statement marker.
   # Using closure witht the button, instead of a subroutine.
   # see Advanced Perl Programming p60.
   my $mv_to_res = sub { 
      $self->move_to_results( $c )
   };


	$rmarkers{$c} = $entry_txt->Button( # -text => '',
         -image => $color_ball{checkmark},
         #-background=>$main::bc,
         -justify => 'center',
         -height => 8,
         -width => 8,
         -highlightthickness => 1,
         -relief => 'raised',
				 -command => $mv_to_res,
      );

	$self->bind_message( $rmarkers{$c}, q{Scroll results window to statement results.} );
	print STDERR "Number $c Mark @ $pl\n" if ( $main::debug > 0 );

	$entry_txt->windowCreate( "$pl.0", -window => $rmarkers{$c} );
	$entry_txt->insert( "$pl.1", " " );
	print STDERR "\n"
         if ( $main::debug > 0 );

	$ind_txt->configure( -state => 'disabled' );
}

#
# Set the current buffer to null (undefined)
#
sub clear_current_buffer {
	my $self = shift;
	foreach (keys %{$self->{c}}) {
		$self->{c}->{$_} = undef;
	}

	undef($c);
	$c = $self->{c};
}

#
# Get the current buffer text.
#

sub get_current_buffer {
   my ($self, $cinx) = @_;
   my $stinx;

	 # First, clear the current buffer.
	 $self->clear_current_buffer();

	 $cinx = 'insert' unless $cinx;
	 # Determine where we are.
   $idxMark = $entry_txt->index( $cinx );

   print STDERR "Search entry txt start: $idxMark "
      if ( $self->debug > 0 );

	 # Search the statement.
   my $inx = $entry_txt->search( 
      -backwards, 
      -regexp, 
      '[;/]$', #',
      $stinx = $entry_txt->index( "$cinx - 2 chars"),
      "1.0",
   );

   # The buffer could have more than one statement in it.
   # Find the last statement. Look for the new statement on
   # the next line.
   $begLn = "1.0";
   if (defined $inx and length($inx) > 0) {
      # Convert the index to something more usable.
      $begLn = $entry_txt->index("$inx + 1 chars"); 
      print STDERR " found at: $inx " if ( $self->debug > 0 );
   } 

	 # Get the current statement.
   $exeStatement = $entry_txt->get( $begLn, $idxMark );
   $entry_txt->tagAdd( 'Exec', $begLn, $idxMark );

	 $c->{beg} = $begLn;
	 $c->{end} = $idxMark;

   print STDERR " getting text from $begLn to $idxMark: $exeStatement\n"
      if ( $self->debug > 0 );

   #
   # The statement determined, now set the begin and end marks.
   #

   my $marks = $self->{marks};
   my $cc = $marks->is_marked($begLn);

   #print Dumper($marks) if ( $self->debug > 0 );

   if (not defined($cc)) {
      print STDERR "C is not defined, creating new mark\n"
         if ( $self->debug > 0 );

      $cc = $marks->mark( $begLn, $idxMark );
			$c->{stat_num} = $cc;
			$c->{beg} = $begLn;
			$self->{n} = $cc;

      print STDERR "Mark created, index $cc\n"
         if ( $self->debug > 0 );

      $entry_txt->markSet( $marks->get_mark_beg($cc),
			$marks->get_beg($cc) );
      $entry_txt->markGravity( $marks->get_mark_beg($cc), "left" );
			$c->{is_marked} = 0;
   } else {
      # Statement is marked already, update the index information.
      $marks->set_mark_beg($cc,$begLn);
      $marks->set_mark_end($cc,$idxMark);
			$c->{stat_num} = $cc;
			$c->{is_marked} = 1;
   }

   print STDERR join( "\n", $entry_txt->markNames(), "" )
      if ( $self->debug > 0 );

   $cc;
}


sub bind_message {
   my $self = shift;
   my ($widget, $msg) = @_;
   $widget->bind('<Enter>', [ sub { $dbistatus = $_[1]; }, $msg ] );
   $widget->bind('<Leave>', sub { $dbistatus = ""; } );
}

sub dbish_clear {
   my $self = shift;

	# Clear the results window.  I may have to release the
	# current marks.
	$rslt_txt->configure( -state => 'normal' );
	$rslt_txt->delete( "1.0", 'end' );
	$rslt_txt->configure( -state => 'disabled' );

	# Clear the entry window.
	$entry_txt->delete( "1.0", 'end' );

	# Clear the indicator window.
	$ind_txt->configure( -state => 'normal' );
	$ind_txt->delete( "1.0", 'end' );
	$ind_txt->configure( -state => 'disabled' );

	# Release the marks structure and rebuild the object.
	$self->{marks} = undef;
	my $marks = Shell::Mark->new;
	$self->{marks} = $marks;

	# Clear the current statement buffer.
	undef($c);
	$c = $self->{c};

	return;
}
   

sub opt_dis_grid {
   my $self = shift;
   if (! $opt_dis_grid ) {
      $self->release();
   }
}

sub red {
   $auto_ball->configure( -image => $color_ball{red} );
}
sub green {
   $auto_ball->configure( -image => $color_ball{green} );
}

sub tba {
   my $self = shift;
   print RSLT_TXT "Work in progress ...\n";
   0;
}

#
# Move to the results of the statement executed.  Currently
# no limit on results stored, but this may change.
#
sub move_to_results {
   my ($self, $c) = @_;
   # Lots of debug information here.
   # print STDERR "Move to Results event for results $c\n";
   # print STDERR join( "\n", $rslt_txt->markNames(), "" );
   # foreach ($rslt_txt->markNames()) {
      # print STDERR "MARK: $_ at ", $rslt_txt->index($_), "\n";
   # }

	if( $rslt_txt->tagRanges( "Bold" ) ) {
		$rslt_txt->tagRemove( 'Bold', $rslt_txt->tagRanges( "Bold" ) );
	}
	 my $rslt_inx =$rslt_txt->index( $self->{marks}->get_results($c));
   $rslt_txt->see( $rslt_inx );
	 $rslt_txt->tagAdd( 'Bold', $rslt_inx, "$rslt_inx lineend" );
}

#
# doit:  Executes either from the execute button or using [/;]
#
sub doit {
	my ($self, $inx) = @_;


	$inx = $entry_txt->index( 'insert lineend' ) unless $inx;

	#print STDERR "doit: $inx\n";

	# get current buffer finds the sql statement.
	$self->get_current_buffer( $inx );

	# now execute it;
	$self->execute_sql();

	{
		local $^W = 0;
		print STDERR "\tStatement executed: ",
		$exeStatement,
		"\n\t\tindex: $inx \n\t\tstatus: ",
		$c->{status}, "\n\t\tmessage: ", $c->{msg},
		"\n"
		if ( $self->debug > 0 );
	}

	# If the execution happened, do a number of things.
	# Mark the results or tag as errored.
	if (!$c->{status}) {
		#my ($start, $end) = $entry_txt->tagNextrange( 'Error', $begLn );
		# Untag the statement if error tagged.
		$self->untag_statement_errored($begLn);
		$self->create_results_btn();
	} else {
		# Change the text to a error.
		$self->tag_statement_errored($begLn);
		# Put up a dialog box with the error message.
		$self->message_dialog("Error Message",
			$c->{msg} . "\n\n$exeStatement\n" ); #"
	}

}

#
# Display a dialog box to the user.  Using this method until
# we create a dialog handler in the core function.
#
sub message_dialog {
	my ($self, $title, $msg) = @_;

	$title = "Message Dialog Box" unless $title;
	$msg = "Well, you called for a message dialog, but didn't give a message"
		unless $msg;
	
	my $d = $dbiwd->Dialog( 
		-title => $title,
		-text => $msg
	);
	return $d->Show;
}

#
#
#
sub search_forward {
	my ($self, $inx) = @_;

	$inx = $entry_txt->index( 'current' ) unless $inx;

	return $entry_txt->search( 
      -regexp, 
      '[;/]$', #',
      $inx,
      "end",
   );
}

#
# As the names implies, this button executes all the statements in the
# current entry buffer.
#
sub execute_all_buffer {
	my ($self) = @_;

	#
	# Start at the top of the buffer and execute each statement.
	#
	my $cinx = '1.0';
	my $done = 0;
	my $i = "";
	while( !$done ) {
		$entry_txt->see( $cinx );
		$i = $self->search_forward( $cinx );
		if ( !defined($i) or length($i) == 0 ) {
			$done = 1;
			last;
		}
		$i = $entry_txt->index( "$i lineend" );

		{
			local $^W = 0;
			print STDERR "\tstart $cinx end $i\n",
				"\tstatement: ",
				$entry_txt->get( $cinx, $i ),
				"\n"
				if ($self->debug > 0);
		}

		$self->doit( $i );
		$cinx = $i;
	};

}

# Execute the most currently statement in the entry buffer.
sub execute_sql {

   my $self = shift;

    $dbiwd->Busy;
    my $statement = $exeStatement;
    chomp $statement;
    $statement =~ s:[/;]$::;  # Replace the last / with nothing.
    
   print STDERR "\nexecuting statement: $statement\n"
      if ( $self->debug > 0 );

   my $sth = $self->do_prepare( $statement );
   $self->no_go("Failed to prepare statement!" ), return undef
	 	if ($c->{status});

   # Statement is prepared, now execute or do.
   my $rv = $self->sth_go( $sth, 1 );

   $self->no_go("Statement execute failed!" ), return undef 
	 	if ($c->{status});

		# OK, at this point mark the results window.
		$self->results_mark();

		$self->display_results($sth);

		#
		# Does the statement require row displayed? If doesn't then
		# show the status from the executed statement.
		#
		$self->no_go( "completed" );
		return $c->{status} = 0 unless $c->{status};
}

sub display_results {
	my ($self, $sth) = @_;

	$self->{dbiwd}->Busy;

	my $sav_state = $rslt_txt->cget( -state );
	$rslt_txt->configure( -state => q{normal} );

	if ($c->{display}) {
		$self->{dbh}->{neat_maxlen} = 40004;
		my $class = $self->formatter($opt_dis_grid);
		my $r = $class->new($self);
		$r->header($sth, \*RSLT_TXT, ",");

		my $row;
		while( $row = $sth->fetchrow_arrayref() ) {
			$r->row($row, \*RSLT_TXT, "," );
		}
		$r->trailer(\*RSLT_TXT);

		$sth->finish;
	} else {
		print RSLT_TXT $c->{msg}, "\n" unless $c->{status};
	}

	$rslt_txt->see( q{end linestart});
	$rslt_txt->configure( -state => $sav_state );
	$self->{dbiwd}->Unbusy;
}

sub no_go {
   my $self = shift;
   $dbistatus = shift;
   $entry_txt->tagRemove( 'Exec', $begLn, $idxMark );
   $rslt_txt->configure( -state => 'disabled' );
   $dbiwd->Unbusy;
   return;
}
sub results_mark {
	my ($self, $stn) = @_;
	my $marks = $self->{marks};
	$stn = $c->{stat_num} unless $stn;
	$marks->set_results($stn);
	$rslt_txt->configure( -state => 'normal' );
	$rslt_txt->markSet( 
		$marks->get_results($stn),
		$rslt_txt->index("insert" )); 
	$rslt_txt->markGravity( 
		$marks->get_results($stn), "left" );
}


# This method is used to track the X text widgets with the scroll bar.
sub sync_txt {
   my $self = shift;
   my ($sb, $scrolled, $lbs, @args) = @_;
   $sb->set(@args);
   my ($top, $bottom) = $scrolled->yview();
   foreach my $list (@$lbs) {
      $list->yviewMoveto($top);
   }
}

sub debug {
	my ($self, $value) = @_;
	$self->{debug} = $value if($value);
$self->{debug};
}

#
# Package Meta:  Meta Data.  It's the data about the data.
#

package Shell::Meta;

sub new {
   my ($proto , $parent) = @_;
   my $class = ref($proto) || $proto;
   my $self  = { };

   bless($self, $class);

   return $self;
}

# DBI does all the hard work on this one.
sub all_tables {
	my ($self, $parent) = @_;

	$parent->{dbiwd}->Busy;
	my $sth = $parent->{dbh}->table_info;
	if ($parent->{dbh}->err) {
		warn qq{Table information is not available: } . 
			$parent->{dbh}->errstr;
	} else {
   $parent->sth_go( $sth, 1 );
	 $parent->display_results( $sth );
	}
	$parent->{dbiwd}->Unbusy;
}

# Handles the creation of the menus.
package Shell::Menu;
use Tk::Pretty;
use Data::Dumper;

sub menu_bar {
   my ($self, $menus_ref) = @_;

	 $menus_ref = $self->{menus} unless $menus_ref;
   $dbistatus = "Creating menu bar";

   my $f = $dbiwd->Frame( -relief => 'ridge', -borderwidth => 2 );

   $f->pack( -side => 'top', -anchor => 'n', -expand => 0, -fill => 'x' );
   
   # Create a menu bar.

   print STDERR "BEFORE menu bar options:\n"
      if ( $self->debug > 0 );

   foreach (@{$menus_ref->{order}}) {
      print STDERR "menu bar options: $_\n"
         if ( $self->debug > 0 );
      $menus_ref->{lc $_ } = $f->Menubutton( -text => $_ , -tearoff => 0 );
			if (m/help/i) {
   			$menus_ref->{lc $_}->pack(-side => 'right' ); # Help
			} else {
   			$menus_ref->{lc $_}->pack(-side => 'left' );  # File
			}
			my $m = qq{menu_} . lc $_;
   		eval {$self->$m($menus_ref->{lc $_}->menu)};
			#warn qq{$@} if $@;
   }
   
   
   print STDERR "menus_ref: >" . $menus_ref . "<\n"
      if ( $self->debug > 0 );

}


sub menu_file {
   my ($self, $menus_ref) = @_;

	 $menus_ref = $self->{menus}->{file}->menu unless $menus_ref;
   print STDERR "menu_file: menus_ref :>" . $menus_ref . "<\n"
      if ( $self->debug > 0 );

   #
   # Add some options to the menus.
   #
   # File menu

   print STDERR "menus-0 (additem) >" . $menus_ref->{file} . "<\n"
      if ( $self->debug > 0 );

   $menus_ref->AddItems(
         [ "command" => "Load", -command => sub { $self->load } ],
         [ "command" => "Save", -command => sub { $self->save } ],
         "-",
         [ "command" => "Properties", -command => sub { $prop->edit } ],
         "-",
         [ "command" => "Exit", -command => sub { $self->exit } ],

                      );
	 #
	 # If Properties state (able to load Storable) is false (0),
	 # disable this menu option.
	 #
	 $menus_ref->entryconfigure( qq{Properties}, 
	 	-state => q{disabled} ) unless $prop->state;
}

sub menu_edit {
   my ($self, $menus_ref) = @_;
}

sub menu_meta {
   my ($self, $menus_ref) = @_;
   $menus_ref->AddItems(
         [ "command" => "All Tables", -command => sub { $meta->all_tables($self) } ],
		);
}

sub menu_options {
   my ($self, $menus_ref) = @_;

   # Options menu

	 my $menu_ref =$self->{menus}->{options}->menu unless $menus_ref;

	 # Autoexecute:  When autoexec is on, statements ending in [;/] will
	 # execute when the enter/return is pressed.

	 $autoexec = 1; # default value is on.
	 $menus_ref->AddItems(
         [ "checkbutton" => "Autoexec", -variable => \$autoexec ],
         [ "checkbutton" => "Debug",    -variable => \$self->{debug} ],
				 "-",
	);
   my $opt_disp = $menus_ref->Menu;
   my @formats = $self->load_formats;
      
   foreach (@formats) {
      $opt_disp->radiobutton( -label => $_, 
         -variable => \$opt_dis_grid,
         -value => $_,
         );
   }

   $menus_ref->cascade( -label => "Display format..."); 
   $menus_ref->entryconfigure( 
	 	"Display format...", -menu => $opt_disp,
	 	-state => q{normal});
   $opt_dis_grid = 'neat';

   # Create the entries for rows returned.

   my $opt_row = $menus_ref->Menu;

   foreach (qw/1 10 25 50 100 all/) {

      $opt_row->radiobutton( -label => $_, -variable => \$opt_row_dis_c,
         -value => $_ );
   }

   $menus_ref->cascade( -label => "Rows return..." );
   $menus_ref->entryconfigure( "Rows return...", 
	 	-menu => $opt_row,
		-state => q{disabled});
   $opt_row_dis_c = 'all';

}

sub menu_help {
   my ($self, $menus_ref) = @_;

	 my $menu_ref =$self->{menus}->{help}->menu unless $menus_ref;

   # Help menu
   $menus_ref->AddItems(
      [ "command" => "Index", -command => sub { $self->tba } ],
      "-",
      [ "command" => "About", -command => sub { $self->tba } ],
   );
}

sub menu_button {

   my $self = shift;

   # Create a button bar.
   $dbistatus = "Creating menu button bar";

   my $bf = $dbiwd->Frame( -relief => 'ridge', -borderwidth => 2 );
   $bf->pack( -side => 'top', -anchor => 'n', -expand => 0, -fill => 'x' );

   # need to invoke the execute in other parts of the application.

   $button_exe = $bf->Button( -text=> 'Execute',
			-image => $color_ball{exec},
			-command=> sub{ $self->doit() }
		)->pack(side=>'left');

	$buttons{exec} = $button_exe;
	$self->bind_message( $buttons{exec}, q{Execute the current statement.} );

   $buttons{execall} = $bf->Button( -text=> 'Execute All',
			-image => $color_ball{execall},
                              -command=> sub{ $self->execute_all_buffer() }
                            )->pack(side=>'left');

	$self->bind_message( $buttons{execall}, q{Execute all the statements in the current entry window.} );

   $buttons{clear} = $bf->Button( -text=> q{Clear},
			-image => $color_ball{clear},
                -command=>sub{ $self->dbish_clear(); }
              )->pack(side=>q{left});

	$self->bind_message( $buttons{clear}, q{Clear entry and results windows.} );

   #$buttons{tables} = $bf->Button( -text=> q{Tables},
                #-command=> sub{ $meta->all_tables($self) }
              #)->pack(side=>q{left});

	#$self->bind_message( $buttons{tables}, q{Display a list of current tables.} );

   #$buttons{copyresults} = $bf->Button( -text=> q{Copy Results},
                #-command=> sub{ $self->tba() },
								#-state => q{disabled}
              #)->pack(side=>'left');

	#$self->bind_message( $buttons{copyresults}, q{Copy the selection results to the clipboard.} );

   $buttons{commit} = $bf->Button( -text=> q{Commit},
			-image => $color_ball{commit},
                -command=> sub{ $self->commit() },
								-state => q{disabled}
              )->pack(side=> q{left});

	$self->bind_message( $buttons{commit}, q{Commit results to the database.} );

   $buttons{rollback} = $bf->Button( -text=> q{Rollback},
			-image => $color_ball{rollback},
                -command=> sub{ $self->rollback() },
								-state => q{disabled}
              )->pack(side=> q{left});

	$self->bind_message( $buttons{rollback}, q{Rollback results from database.} );

   # Put the logo on this bar.

   my $orac_logo = $dbiwd->Photo(-file=>"$FindBin::RealBin/img/orac.gif");

   $bf->Label(-image=> $orac_logo, 
              -borderwidth=> 2,
              -relief=> 'flat'

             )->pack(  -side=> 'right', 
                       -anchor=>'e', 
                       -expand => 0, 
                       -fill => 'x'
                    );
   
   $dbistatus = "Creating Close button";
   
   $buttons{close} = $bf->Button( -text => "Close",
               -command => sub { 
 
                 $dbiwd->withdraw;
                 $self->{mw}->deiconify();

                               } 
              )->pack( -side => qq{right} );


   $self->bind_message( $buttons{close}, q{As my children like to say "See you;Wouldn't want to be ya"});

}

#
# Properties allow the user to store setting.
#
package Shell::Properties;

my $state = 1;
eval qq{ use Storable };
$state = 0 if $@;

sub state {
	$state;
}

# Disable menu pick, if $state is 0
sub new {
   my ($proto , $parent) = @_;
   my $class = ref($proto) || $proto;
	 my $options = $parent->{options};
   my $self  = {
       options => \$options,
    };

   bless($self, $class);

   return $self;
}

sub display {

}

sub save {

}

sub load {

}

sub edit {

}

package Shell::File;
use Tk::FileSelect;

#$FSref = $top->FileSelect(-directory => $start_dir);
              #$top            - a window reference, e.g. MainWindow->new
              #$start_dir      - the starting point for the FileSelect
#$file = $FSref->Show;
              #Executes the fileselector until either a filename is
              #accepted or the user hits Cancel. Returns the filename
              #or the empty string, respectively, and unmaps the
              #FileSelect.
#$FSref->configure(option => value[, ...])
              #Please see the Populate subroutine as the configuration
              #list changes rapidly.

sub get_file {
	my $self = shift;
	my $fs = $dbiwd->FileSelect( -directory => "$main::orac_home/sql" );
	return $fs->Show;
}

sub save {
	my $self = shift;

	my $dialog = $dbiwd->Dialog(
		-text => 'Save Current Statement or Current Entry Buffer?',
		-bitmap => 'question',
		-title => 'Save File Dialog',
		-default_button => 'Statement',
		-buttons => [qw/Statement Buffer Cancel/]
	);

	my $ans = $dialog->Show();

	return if ($ans =~ m/Cancel/i);

	my $statement = $exeStatement;
	$statement = $entry_txt->get( '1.0', 'end' ) if ($ans =~ m/Buffer/i);

	my $file = $self->get_file();

	# User presses the cancel button.
	return undef unless ( length($file) > 0 );

	my $confirm;
	if (-f $file) {
		$confirm = $dbiwd->Dialog(
			-text => "File exists!  Overwrite current file\n${file}?",
			-bitmap => 'warning',
			-title => "Confirm overwrite",
			-default_button => 'No',
			-buttons => [ qw/Yes No Cancel/ ]
		);

		my $ans = $confirm->Show();

		return undef if ($ans =~ m/Cancel/i);
	}

	open( SAVE_FILE, "> ${file}" ) || do {
		warn "Unable to save to ${file}: $!\n";
		return undef;
	};

	print SAVE_FILE "${statement}\n";

	return close( SAVE_FILE );
}

sub load {
	my $self = shift;

	my $file = $self->get_file;
	# User presses the cancel button.
	return undef unless ( defined($file) and length($file) > 0 );

	print STDERR "Loading buffer with $file\n"
      if ( $self->debug > 0 );

	print STDERR "buffer: 1.0 -> ", $entry_txt->index( 'end' ), "\n"
      if ( $self->debug > 0 );
	my $ans = 'Replace';
	if ($self->is_empty) {
	my $dialog = $dbiwd->Dialog(
		-text => 'Append or Replace current entry buffer?',
		-bitmap => 'question',
		-title => 'Load File Dialog',
		-default_button => 'Append',
		-buttons => [qw/Append Replace Cancel/]);

		$ans = $dialog->Show;

		return if $ans =~ m/Cancel/i;
	}

	$self->dbish_clear if $ans =~ m/Replace/i;

	open( LOAD_FILE, "<$file" ) || do {
		warn "Unable to open $file: $!\n";
		return undef;
	};

	while( <LOAD_FILE> ) {
		$entry_txt->insert( 'end', $_ );
	}

	return close( LOAD_FILE );
}

sub is_empty {
	my $self = shift;

	my $txt = $entry_txt->get( '1.0', 'end' );
	chomp $txt;
	return length( $txt );
}
sub view {
	my $self = shift;

}

package Shell::Mark;
use strict;
my $statement_count = 1;

my %marks;

sub mark {
   my ( $self, $begin, $end, $mark_beg, $mark_end, $status,
      $results, $index ) = @_;

   my $cur = "sql_" . $statement_count;
   $mark_beg = $cur . "_beg" unless $mark_beg;
   $mark_end = $cur . "_end" unless $mark_end;

   $marks{$statement_count} =  {
      "begin" => $begin,
      "end"   => $end,
      "mark_beg" => $mark_beg,
      "mark_end" => $mark_end,
      "status" => $status,
      "results" => $results,
      "count" => $statement_count,
   };



   return $statement_count++;
}

sub remove {
   my ($self, $mark_inx) = @_;
   if (exists $marks{$mark_inx}) {
      delete $marks{$mark_inx} && $self->{count}--;
   }
}

sub is_marked {
   my ($self, $index) = @_;
   foreach (keys %marks) {
      return $_ 
         if ($marks{$_}->{begin} eq $index);
   }
   return undef;

};

sub get_beg {
   my ($self, $inx) = @_;
   $marks{$inx}->{begin};
}

sub set_beg {
   my ($self, $inx, $val) = @_;
   $marks{$inx}->{begin} = $val;
}

sub get_mark_beg {
   my ($marks, $inx) = @_;
   $marks{$inx}->{mark_beg};
}

sub set_mark_beg {
   my ($self, $inx, $val) = @_;
   $marks{$inx}->{mark_beg} = $val;
}


sub get_end {
   my ($self, $inx) = @_;
   $marks{$inx}->{end};
}

sub set_end {
   my ($self, $inx, $val) = @_;
   $marks{$inx}->{end} = $val;
}

sub get_mark_end {
   my ($self, $inx) = @_;
   $marks{$inx}->{mark_end};
}

sub set_mark_end {
   my ($self, $inx, $val) = @_;
   $marks{$inx}->{mark_end} = $val;
}

# Set/Get the results mark name.
sub set_results {
   my ($self, $inx, $val) = @_;
   $marks{$inx}->{results} = "rslt_" . $inx . "_beg";
}

sub get_results {
   my ($self, $inx, $val) = @_;
   $marks{$inx}->{results};
}

sub new {

   my $proto = shift;
   my $class = ref($proto) || $proto;
   my $self  = {
       count => \$statement_count,
      mark => undef,
    };

   bless($self, $class);

	 $statement_count = 1;
	 undef(%marks);

   return $self;
}

1;
