#!/usr/bin/env perl

use strict;
use warnings;

use Test::More tests => 9;

BEGIN {
    use_ok('Carp');
    use_ok('WWW::Rafb::Create');
    use_ok('POE');
    use_ok('POE::Filter::Line');
    use_ok('POE::Filter::Reference');
    use_ok('POE::Wheel::Run');
	use_ok( 'POE::Component::WWW::Rafb::Create' );
}

diag( "Testing POE::Component::WWW::Rafb::Create $POE::Component::WWW::Rafb::Create::VERSION, Perl $], $^X" );

use POE qw(Component::WWW::Rafb::Create);

my $o = POE::Component::WWW::Rafb::Create->spawn( debug => 1);
isa_ok( $o, 'POE::Component::WWW::Rafb::Create');
can_ok($o, qw(spawn shutdown paste session_id _start _sig_child
                    _paste _shutdown _child_closed _child_error
                    _child_stderr _child_stdout _wheel _process_request) );

POE::Session->create( inline_states => { _start => sub { $o->shutdown; } } );
$poe_kernel->run;