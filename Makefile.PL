use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'SQL::Yapp',
    AUTHOR              => q{Henrik Theiling <contact@theiling.de>},
    VERSION_FROM        => 'lib/SQL/Yapp.pm',
    ABSTRACT_FROM       => 'lib/SQL/Yapp.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean               => { FILES => 'SQL-Yapp-*' },
);
