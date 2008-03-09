#!/usr/bin/env perl

use strict;
use warnings;

die "Usage: perl retrieve.pl <file_to_paste>\n"
    unless @ARGV;

open my $fh, '<', $ARGV[0]
    or die "Failed to open `$ARGV[0]` ($!)";

my $Text = do { local $/; <$fh>; };

use lib '../lib';
use POE qw(Component::WWW::Rafb::Create);

my $poco = POE::Component::WWW::Rafb::Create->spawn;

POE::Session->create(
    package_states => [ main => [qw(_start pasted )] ],
);

$poe_kernel->run;

sub _start {
    $poco->paste( {
            text    => $Text,
            event => 'pasted',
        }
    );
}

sub pasted {
    my $in = $_[ARG0];
    if ( $in->{error} ) {
        print "Got an error: $in->{error}\n";
    }
    else {
        print "Paste can be found on $in->{uri}\n";
    }
    $poco->shutdown;
}

