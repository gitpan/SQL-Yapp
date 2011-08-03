use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

sub MY::postamble {
    return <<'EOT'
blib/html/SQL/Yapp.html: lib/SQL/Yapp.pm
	mkdir -p blib/html/SQL
	pod2html --title 'Perl module SQL::Yapp.pm' --infile=$< --outfile=blib/html/SQL/Yapp.html

pm_to_blib: blib/html/SQL/Yapp.html
EOT
}

WriteMakefile(
    NAME                => 'SQL::Yapp',
    ABSTRACT            => 'SQL syntax in Perl with compile-time syntax checks',
    AUTHOR              => 'Henrik Theiling <cpan@theiling.de>',
    VERSION_FROM        => 'lib/SQL/Yapp.pm',
    ABSTRACT_FROM       => 'lib/SQL/Yapp.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()
    ),
    PL_FILES            => {},
    PREREQ_PM => {
        'Carp'           => 0,
        'Data::Dumper'   => 0,
        'Filter::Simple' => 0,
        'Hash::Util'     => 0,
        'Scalar::Util'   => 0,
        'Test::More'     => 0,
        'Text::Balanced' => 0,
    },
    dist   => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean  => { FILES => 'SQL-Yapp-*' },
);
