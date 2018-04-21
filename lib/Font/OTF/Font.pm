package Font::OTF::Font;

=head1 NAME

Font::OTF::Font - Memory representation of a font

=head1 SYNOPSIS

Here is the regression test (you provide your own font). Run it once and then
again on the output of the first run. There should be no differences between
the outputs of the two runs.

    $f = Font::OTF::Font->open($ARGV[0]);

    # force a read of all the tables
    $f->tables_do(sub { $_[0]->read; });

    # force read of all glyphs
    $f->{'loca'}->glyphs_do(sub { $_[0]->read_dat; });
    # NB. no need to $g->update since $f->{'glyf'}->out will do it for us

    $f->out($ARGV[1]);
    $f->release;            # clear up memory forcefully

=head1 DESCRIPTION

A Truetype font consists of a header containing a directory of tables which
constitute the rest of the file. This class holds that header and directory and
also creates objects of the appropriate type for each table within the font.
Note that it does not read each table into memory, but creates a short reference
which can be read using the form:

    $f->{$tablename}->read;

Classes are included that support many of the different TrueType tables. For
those for which no special code exists, the table type C<table> is used, which
defaults to L<Font::OTF::Table>. The current tables which are supported are:

    table       Font::OTF::Table      - for unknown tables
    EBDT        Font::OTF::EBDT
    EBLC        Font::OTF::EBLC
    Feat        Font::OTF::GrFeat
    GDEF        Font::OTF::GDEF
    GPOS        Font::OTF::GPOS
    GSUB        Font::OTF::GSUB
    Glat        Font::OTF::Glat
    Gloc        Font::OTF::Gloc
    LTSH        Font::OTF::LTSH
    OS/2        Font::OTF::OS_2
    PCLT        Font::OTF::PCLT
    Sill        Font::OTF::Sill
    Silf        Font::OTF::Silf
    bsln        Font::OTF::Bsln
    cmap        Font::OTF::Cmap       - see also Font::OTF::OldCmap
    cvt         Font::OTF::Cvt_
    fdsc        Font::OTF::Fdsc
    feat        Font::OTF::Feat
    fmtx        Font::OTF::Fmtx
    fpgm        Font::OTF::Fpgm
    glyf        Font::OTF::Glyf       - see also Font::OTF::Glyph
    hdmx        Font::OTF::Hdmx
    head        Font::OTF::Head
    hhea        Font::OTF::Hhea
    hmtx        Font::OTF::Hmtx
    kern        Font::OTF::Kern       - see alternative Font::OTF::AATKern
    loca        Font::OTF::Loca
    maxp        Font::OTF::Maxp
    mort        Font::OTF::Mort       - see also Font::OTF::OldMort
    name        Font::OTF::Name
    post        Font::OTF::Post
    prep        Font::OTF::Prep
    prop        Font::OTF::Prop
    vhea        Font::OTF::Vhea
    vmtx        Font::OTF::Vmtx
    DSIG        Font::OTF::DSIG

Links are:

L<Font::OTF::Table>
L<Font::OTF::EBDT> L<Font::OTF::EBLC> L<Font::OTF::GrFeat>
L<Font::OTF::GDEF> L<Font::OTF::GPOS> L<Font::OTF::GSUB> L<Font::OTF::Glat> L<Font::OTF::Gloc> L<Font::OTF::LTSH>
L<Font::OTF::OS_2> L<Font::OTF::PCLT> L<Font::OTF::Sill> L<Font::OTF::Silf> L<Font::OTF::Bsln> L<Font::OTF::Cmap> L<Font::OTF::Cvt_>
L<Font::OTF::Fdsc> L<Font::OTF::Feat> L<Font::OTF::Fmtx> L<Font::OTF::Fpgm> L<Font::OTF::Glyf>
L<Font::OTF::Hdmx> L<Font::OTF::Head> L<Font::OTF::Hhea> L<Font::OTF::Hmtx> L<Font::OTF::Kern>
L<Font::OTF::Loca> L<Font::OTF::Maxp> L<Font::OTF::Mort> L<Font::OTF::Name> L<Font::OTF::Post>
L<Font::OTF::Prep> L<Font::OTF::Prop> L<Font::OTF::Vhea> L<Font::OTF::Vmtx> L<Font::OTF::OldCmap>
L<Font::OTF::Glyph> L<Font::OTF::AATKern> L<Font::OTF::OldMort>
L<Font::OTF::DSIG>


=head1 INSTANCE VARIABLES

Instance variables begin with a space (and have lengths greater than the 4
characters which make up table names).

=over

=item nocsum

This is used during output to disable the creation of the file checksum in the
head table. For example, during DSIG table creation, this flag will be set to
ensure that the file checksum is left at zero.

=item noharmony

If set, do not harmonize the script and lang trees of GPOS and GSUB tables. See L<Font::OTF::Ttopen> for more info.

=item nocompress

Is the default value controlling WOFF output table compression. If undef, all tables will be compressed if there is
a size benefit in doing so.
It may be set to an array of tagnames naming tables that should not be compressed, or to a scalar integer specifying a
table size threshold below which tables will not be compressed.
Note that individual L<Font::OTF::Table> objects may override this default. See L<Font::OTF::Table> for more info.

=item fname (R)

Contains the filename of the font which this object was read from.

=item INFILE (P)

The file handle which reflects the source file for this font.

=item OFFSET (P)

Contains the offset from the beginning of the read file of this particular
font directory, thus providing support for TrueType Collections.

=item WOFF

Contains a reference to a C<Font::OTF::Woff> object.

=back

=head1 METHODS

=cut

use IO::File;

use strict;
use vars qw(%tables $VERSION $dumper);
use Symbol();

require 5.006;

my $havezlib = eval {require Compress::Zlib};

$VERSION = 0.01;

%tables = (
        'table' => 'Font::OTF::Table',
        'DSIG' => 'Font::OTF::DSIG',
        'EBDT' => 'Font::OTF::EBDT',
        'EBLC' => 'Font::OTF::EBLC',
        'Feat' => 'Font::OTF::GrFeat',
        'GDEF' => 'Font::OTF::GDEF',
        'Glat' => 'Font::OTF::Glat',
        'Gloc' => 'Font::OTF::Gloc',
        'GPOS' => 'Font::OTF::GPOS',
        'GSUB' => 'Font::OTF::GSUB',
        'Glat' => 'Font::OTF::Glat',
        'Gloc' => 'Font::OTF::Gloc',
        'LTSH' => 'Font::OTF::LTSH',
        'OS/2' => 'Font::OTF::OS_2',
        'PCLT' => 'Font::OTF::PCLT',
        'Sill' => 'Font::OTF::Sill',
        'Silf' => 'Font::OTF::Silf',
        'bsln' => 'Font::OTF::Bsln',
        'cmap' => 'Font::OTF::Cmap',
        'cvt ' => 'Font::OTF::Cvt_',
        'fdsc' => 'Font::OTF::Fdsc',
        'feat' => 'Font::OTF::Feat',
        'fmtx' => 'Font::OTF::Fmtx',
        'fpgm' => 'Font::OTF::Fpgm',
        'glyf' => 'Font::OTF::Glyf',
        'hdmx' => 'Font::OTF::Hdmx',
        'head' => 'Font::OTF::Head',
        'hhea' => 'Font::OTF::Hhea',
        'hmtx' => 'Font::OTF::Hmtx',
        'kern' => 'Font::OTF::Kern',
        'loca' => 'Font::OTF::Loca',
        'maxp' => 'Font::OTF::Maxp',
        'mort' => 'Font::OTF::Mort',
        'name' => 'Font::OTF::Name',
        'post' => 'Font::OTF::Post',
        'prep' => 'Font::OTF::Prep',
        'prop' => 'Font::OTF::Prop',
        'vhea' => 'Font::OTF::Vhea',
        'vmtx' => 'Font::OTF::Vmtx',
          );

# This is special code because I am fed up of every time I x a table in the debugger
# I get the whole font printed. Thus substitutes my 3 line change to dumpvar into
# the debugger. Clunky, but nice. You are welcome to a copy if you want one.

BEGIN {
    my ($p);

    for $p (@INC) {
        if (-f "$p/mydumpvar.pl") {
            $dumper = 'mydumpvar.pl';
            last;
        }
    }
    $dumper ||= 'dumpvar.pl';
}

sub main::dumpValue { do $dumper; &main::dumpValue; }


=head2 Font::OTF::Font->AddTable($tablename, $class)

Adds the given class to be used when representing the given table name. It also
'requires' the class for you.

=cut

sub AddTable {
    my ($class, $table, $useclass) = @_;

    $tables{$table} = $useclass;
#    $useclass =~ s|::|/|oig;
#    require "$useclass.pm";
}


=head2 Font::OTF::Font->Init

For those people who like making fonts without reading them. This subroutine
will require all the table code for the various table types for you. Not
needed if using Font::OTF::Font::read before using a table.

=cut

sub Init {
    my ($class) = @_;
    my ($t);

    for $t (values %tables) {
        $t =~ s|::|/|oig;
        require "$t.pm";
    }
}

=head2 Font::OTF::Font->new(%props)

Creates a new font object and initialises with the given properties. This is
primarily for use when a TTF is embedded somewhere. Notice that the properties
are automatically preceded by a space when inserted into the object. This is in
order that fields do not clash with tables.

=cut


sub new {
    my ($class, %props) = @_;
    my ($self) = {};

    bless $self, $class;

    for (keys %props) { $self->{" $_"} = $props{$_}; }
    $self;
}


=head2 Font::OTF::Font->open($fname)

Reads the header and directory for the given font file and creates appropriate
objects for each table in the font.

=cut

sub open {
    my ($class, $fname) = @_;
    my ($fh);
    my ($self) = {};

    unless (ref($fname)) {
        $fh = IO::File->new($fname) or return undef;
        binmode $fh;
    }
    else { $fh = $fname; }

    $self->{' INFILE'} = $fh;
    $self->{' fname'} = $fname;
    $self->{' OFFSET'} = 0;
    bless $self, $class;

    $self->read;
}

# see also: fontTools/tty.py->guessFileType() XXX: wrong place for testing XML

sub _header {
  my ($self, $head) = @_;

  my $sigs = [
	{'sig' => unpack('N', 'snft'), 'type' => 'TTF',  'desc' => 'TTF'},
	{'sig' => unpack('N', 'FFIL'), 'type' => 'TTF',  'desc' => 'TTF'},
	{'sig' => unpack('N', 'OTTO'), 'type' => 'OTTO', 'desc' => 'CFF data (v1 or v2)'},
	{'sig' => unpack('N', 'ttcf'), 'type' => 'TTC',  'desc' => 'TrueType Collection'},
	{'sig' => x0100,               'type' => 'x010', 'desc' => 'TrueType outlines'},
	{'sig' => unpack('N', 'true'), 'type' => 'true', 'desc' => 'Apple TTF'},
	{'sig' => unpack('N', 'typ1'), 'type' => 'TTF',  'desc' => 'Apple TTF'},
	{'sig' => unpack('N', 'wOFF'), 'type' => 'wOFF', 'desc' => 'wOFF'},
	{'sig' => unpack('N', 'wOF2'), 'type' => 'wOF2', 'desc' => 'wOF2'},
	{'sig' => xefbbbf3c,           'type' => 'BOM',  'desc' => 'maybe XML'},
	{'sig' => unpack('N', '<?xm'), 'type' => 'xml',  'desc' => 'maybe OTX or TTX'},
  ];

  for my $sig (@{$sigs}) {
    if ($head == $sig->{'sig'}) { return $sig->{'type'} }
  }
  return '';
}


=head2 $f->read

Reads a Truetype font directory starting from location C<$self->{' OFFSET'}> in the file.
This has been separated from the C<open> function to allow support for embedded
TTFs for example in TTCs. Also reads the C<head> and C<maxp> tables immediately.

=cut

sub read {
    my ($self) = @_;
    my ($fh) = $self->{' INFILE'};
    my ($dat, $i, $ver, $dir_num, $type, $name, $check, $off, $len, $t);
    my ($iswoff, $woffLength, $sfntSize, $zlen);    # needed for WOFF files

    $fh->seek($self->{' OFFSET'}, 0);
    $fh->read($dat, 4);
    $ver = unpack("N", $dat);
    #$iswoff = ($ver == unpack('N', 'wOFF'));
    $iswoff = ($self->_header($ver) eq 'wOFF'));
    if ($iswoff) {
        require Font::OTF::Woff;
        my $woff = Font::OTF::Woff->new(PARENT  => $self);
        $fh->read($dat, 32);
        ($ver, $woffLength, $dir_num, undef, $sfntSize, $woff->{'majorVersion'}, $woff->{'minorVersion'},
            $off, $zlen, $len) = unpack('NNnnNnnNNN', $dat);
        # TODO: According to WOFF spec we should verify $woffLength and $sfntSize, and fail if the values are wrong.
        if ($off) {
            # Font has metadata
            if ($off + $zlen > $woffLength) {
                warn "invalid WOFF header in $self->{' fname'}: meta data beyond end.";
                return undef;
            }
            require Font::OTF::Woff::MetaData;
            $woff->{'metaData'} = Font::OTF::Woff::MetaData->new(
                PARENT     => $woff,
                INFILE     => $fh,
                OFFSET     => $off,
                LENGTH     => $len,
                ZLENGTH    => $zlen);
        }

        $fh->read($dat, 8);
        ($off, $len) = unpack('NN', $dat);
        if ($off) {
            # Font has private data
            if ($off + $len > $woffLength) {
                warn "invalid WOFF header in $self->{' fname'}: private data beyond end.";
                return undef;
            }
            require Font::OTF::Woff::PrivateData;
            $woff->{'privateData'} = Font::OTF::Woff::PrivateData->new(
                PARENT     => $woff,
                INFILE     => $fh,
                OFFSET     => $off,
                LENGTH     => $len);
        }

        $self->{' WOFF'} = $woff;
    }
    else {
        $fh->read($dat, 8);
        $dir_num = unpack("n", $dat);
    }

    $self->_header($ver)    eq 'x010'   # TrueType outlines
    || $self->_header($ver) eq 'OTTO')  # 0x4F54544F CFF outlines
    || $self->_header($ver) eq 'true')  # 0x74727565 Mac sfnts
    or return undef;                    # unsupported type # TODO: Error message


    for ($i = 0; $i < $dir_num; $i++) {
        $fh->read($dat, $iswoff ? 20 : 16) || die "Reading table entry";
        if ($iswoff) {
            ($name, $off, $zlen, $len, $check) = unpack("a4NNNN", $dat);
            if ($off + $zlen > $woffLength || $zlen > $len) {
                my $err;
                $err = "Offset + compressed length > total length. " if $off + $zlen > $woffLength;
                $err = "Compressed length > uncompressed length. " if $zlen > $len;
                warn "invalid WOFF '$name' table in $self->{' fname'}: $err\n";
                return undef;
            }
        }
        else {
            ($name, $check, $off, $len) = unpack("a4NNN", $dat);
            $zlen = $len;
        }
        $self->{$name} = $self->{' PARENT'}->find($self, $name, $check, $off, $len) and next
                if (defined $self->{' PARENT'});
        $type = $tables{$name} || 'Font::OTF::Table';
        $t = $type;
        if ($^O eq "MacOS") { $t =~ s/^|::/:/oig; }
        else { $t =~ s|::|/|oig; }
        require "$t.pm";
        $self->{$name} = $type->new(PARENT  => $self,
                                    NAME    => $name,
                                    INFILE  => $fh,
                                    OFFSET  => $off,
                                    LENGTH  => $len,
                                    ZLENGTH => $zlen,
                                    CSUM    => $check);
    }

    for $t ('head', 'maxp') { $self->{$t}->read if defined $self->{$t}; }

    $self;
}


=head2 $f->out($fname [, @tablelist])

Writes a TTF file consisting of the tables in tablelist. The list is checked to
ensure that only tables that exist are output. (This means that you cannot have
non table information stored in the font object with key length of exactly 4)

In many cases the user simply wants to output all the tables in alphabetical order.
This can be done by not including a @tablelist, in which case the subroutine will
output all the defined tables in the font in alphabetical order.

Returns $f on success and undef on failure, including warnings.

All output files must include the C<head> table.

=cut

sub out {
    my ($self, $fname, @tlist) = @_;
    my ($fh);
    my ($dat, $numTables, $sRange, $eSel);
    my (%dir, $k, $mloc, $count);
    my ($csum, $lsum, $msum, $loc, $oldloc, $len, $shift);

    my ($iswoff); # , $woffLength, $sfntSize, $zlen);   # needed for WOFF files

    unless (ref($fname)) {
        $fh = IO::File->new("+>$fname") || return warn("Unable to open $fname for writing"), undef;
        binmode $fh;
    } else
    { $fh = $fname; }

    $self->{' oname'} = $fname;
    $self->{' outfile'} = $fh;

    if ($self->{' wantsig'}) {
        $self->{' nocsum'} = 1;
#        $self->{'head'}{'checkSumAdjustment'} = 0;
        $self->{' tempDSIG'} = $self->{'DSIG'};
        $self->{' tempcsum'} = $self->{'head'}{' CSUM'};
        delete $self->{'DSIG'};
        @tlist = sort {$self->{$a}{' OFFSET'} <=> $self->{$b}{' OFFSET'}}
            grep (length($_) == 4 && defined $self->{$_}, keys %$self) if ($#tlist < 0);
    }
    elsif ($#tlist < 0) { @tlist = sort keys %$self; }

    @tlist = grep(length($_) == 4 && defined $self->{$_}, @tlist);
    $numTables = $#tlist + 1;
    $numTables++ if ($self->{' wantsig'});

    if ($iswoff) { }
    else {
        ($numTables, $sRange, $eSel, $shift) = Font::OTF::Utils::TTF_bininfo($numTables, 16);
        $dat = pack("Nnnnn", 1 << 16, $numTables, $sRange, $eSel, $shift);
        $fh->print($dat);
        $msum = unpack("%32N*", $dat);
    }

# reserve place holders for each directory entry
    for $k (@tlist) {
        $dir{$k} = pack("A4NNN", $k, 0, 0, 0);
        $fh->print($dir{$k});
    }

    $fh->print(pack('A4NNN', '', 0, 0, 0)) if ($self->{' wantsig'});

    $loc = $fh->tell();
    if ($loc & 3) {
        $fh->print(substr("\000" x 4, $loc & 3));
        $loc += 4 - ($loc & 3);
    }

    for $k (@tlist) {
        $oldloc = $loc;
        if ($iswoff && $havezlib &&
            # output font is WOFF -- should we try to compress this table?
            exists ($self->{$k}->{' nocompress'}) ? $self->{$k}->{' nocompress'} != -1 :
            ref($self->{' nocompress'}) eq 'ARRAY' ? !exists($self->{' nocompress'}{$k}) :
            ref($self->{' nocompress'}) eq 'SCALAR' && $self->{' nocompress'} != -1) {
            # Yes -- we may want to compress this table.
            # Create string file handle to hold uncompressed table
            my $dat;
            my $fh2 = IO::String->new($dat);
            binmode $fh2;
            $self->{$k}->out($fh2);
            $len = $fh2->tell();
            close $fh2;

            # Is table long enough to try compression?
            unless (
                exists ($self->{$k}->{' nocompress'})
                && $len <= $self->{$k}->{' nocompress'}
                || ref($self->{' nocompress'}) eq 'SCALAR'
                && $len <= $self->{' nocompress'}) {
                # Yes -- so compress and check lengths:
                my $zdat = Compress::Zlib::compress($dat);
                my $zlen = bytes::length($zdat);

                if ($zlen < $len) {
                    # write the compressed $zdat
                }
                else {
                    # write the uncompressed $dat
                }
            }
            else {
                # write uncompressed $dat
            }


        }
        else {
            # Output table normally
            $self->{$k}->out($fh);
            $loc = $fh->tell();
            $len = $loc - $oldloc;
        }
        if ($loc & 3) {
            $fh->print(substr("\000" x 4, $loc & 3));
            $loc += 4 - ($loc & 3);
        }
        $fh->seek($oldloc, 0);
        $csum = 0; $mloc = $loc;
        while ($mloc > $oldloc) {
            $count = ($mloc - $oldloc > 4096) ? 4096 : $mloc - $oldloc;
            $fh->read($dat, $count);
            $csum += unpack("%32N*", $dat);
# this line ensures $csum stays within 32 bit bounds, clipping as necessary
            if ($csum > 0xffffffff) { $csum -= 0xffffffff; $csum--; }
            $mloc -= $count;
        }
        $dir{$k} = pack("A4NNN", $k, $csum, $oldloc, $len);
        $msum += $csum + unpack("%32N*", $dir{$k});
        while ($msum > 0xffffffff) { $msum -= 0xffffffff; $msum--; }
        $fh->seek($loc, 0);
    }

    unless ($self->{' nocsum'}) {            # assuming we want a file checksum

        # Now we need to sort out the head table's checksum
        if (!defined $dir{'head'}) {         # you have to have a head table
            $fh->close();
            return warn("No 'head' table to output in $fname"), undef;
        }
        ($csum, $loc, $len) = unpack("x4NNN", $dir{'head'});
        $fh->seek($loc + 8, 0);
        $fh->read($dat, 4);
        $lsum = unpack("N", $dat);
        if ($lsum != 0) {
            $csum -= $lsum;
            if ($csum < 0) { $csum += 0xffffffff; $csum++; }
            $msum -= $lsum * 2;                     # twice (in head and in csum)
            while ($msum < 0) { $msum += 0xffffffff; $msum++; }
        }
        $lsum = 0xB1B0AFBA - $msum;
        $fh->seek($loc + 8, 0);
        $fh->print(pack("N", $lsum));
        $dir{'head'} = pack("A4NNN", 'head', $csum, $loc, $len);
    } elsif ($self->{' wantsig'}) {
        if (!defined $dir{'head'}) {                                   # you have to have a head table
            $fh->close();
            return warn("No 'head' table to output in $fname"), undef;
        }
        ($csum, $loc, $len) = unpack("x4NNN", $dir{'head'});
        $fh->seek($loc + 8, 0);
        $fh->print(pack("N", 0));
#        $dir{'head'} = pack("A4NNN", 'head', $self->{' tempcsum'}, $loc, $len);
    }

# Now we can output the directory again
    if ($self->{' wantsig'}) { @tlist = sort @tlist; }
    $fh->seek(12, 0);
    for $k (@tlist) { $fh->print($dir{$k}); }
    $fh->print(pack('A4NNN', '', 0, 0, 0)) if ($self->{' wantsig'});
    $fh->close();
    $self;
}


=head2 $f->out_xml($filename [, @tables])

Outputs the font in XML format

=cut

sub out_xml {
    my ($self, $fname, @tlist) = @_;
    my ($fh, $context, $numTables, $k);

    $context->{'indent'} = ' ' x 4;

    unless (ref($fname)) {
        $fh = IO::File->new("+>$fname") || return warn("Unable to open $fname"), undef;
        binmode $fh;
    }
    else { $fh = $fname; }

    unless (scalar @tlist > 0) {
        @tlist = sort keys %$self;
        @tlist = grep(length($_) == 4 && defined $self->{$_}, @tlist);
    }
    $numTables = $#tlist + 1;

    $context->{'fh'} = $fh;
    $fh->print("<?xml version='1.0' encoding='UTF-8'?>\n");
    $fh->print("<font tables='$numTables'>\n\n");

    for $k (@tlist) {
        $fh->print("<table name='$k'>\n");
        $self->{$k}->out_xml($context, $context->{'indent'});
        $fh->print("</table>\n");
    }

    $fh->print("</font>\n");
    $fh->close;
    $self;
}


=head2 $f->XML_start($context, $tag, %attrs)

Handles start messages from the XML parser. Of particular interest to us are <font> and
<table>.

=cut

sub XML_start {
    my ($self, $context, $tag, %attrs) = @_;
    my ($name, $type, $t);

    if ($tag eq 'font') { $context->{'tree'}[-1] = $self; }
    elsif ($tag eq 'table') {
        $name = $attrs{'name'};
        unless (defined $self->{$name}) {
            $type = $tables{$name} || 'Font::OTF::Table';
            $t = $type;
            if ($^O eq "MacOS") { $t =~ s/^|::/:/oig; }
            else { $t =~ s|::|/|oig; }
            require "$t.pm";
            $self->{$name} = $type->new('PARENT' => $self, 'NAME' => $name, 'read' => 1);
        }
        $context->{'receiver'} = ($context->{'tree'}[-1] = $self->{$name});
    }
    $context;
}


sub XML_end {
    my ($self) = @_;
    my ($context, $tag, %attrs) = @_;
    my ($i);

    return undef unless ($tag eq 'table' && $attrs{'name'} eq 'loca');
    if (defined $context->{'glyphs'} && $context->{'glyphs'} ne $self->{'loca'}{'glyphs'}) {
        for ($i = 0; $i <= $#{$context->{'glyphs'}}; $i++) {
            $self->{'loca'}{'glyphs'}[$i] = $context->{'glyphs'}[$i] if defined $context->{'glyphs'}[$i];
        }
        $context->{'glyphs'} = $self->{'loca'}{'glyphs'};
    }
    return undef;
}

=head2 $f->update

Sends update to all the tables in the font and then resets all the isDirty
flags on each table. The data structure in now consistent as a font (we hope).

=cut

sub update {
    my ($self) = @_;

    $self->tables_do(sub { $_[0]->update; });

    $self;
}

=head2 $f->dirty

Dirties all the tables in the font

=cut

sub dirty { $_[0]->tables_do(sub { $_[0]->dirty; }); $_[0]; }

=head2 $f->tables_do(&func [, tables])

Calls &func for each table in the font. Calls the table in alphabetical sort
order as per the order in the directory:

    &func($table, $name);

May optionally take a list of table names in which case func is called
for each of them in the given order.

=cut

sub tables_do {
    my ($self, $func, @tables) = @_;
    my ($t);

    for $t (@tables ? @tables : sort grep {length($_) == 4} keys %$self) { &$func($self->{$t}, $t); }
    $self;
}


=head2 $f->release

Releases ALL of the memory used by the TTF font and all of its component
objects.  After calling this method, do B<NOT> expect to have anything left in
the C<Font::OTF::Font> object.

B<NOTE>, that it is important that you call this method on any
C<Font::OTF::Font> object when you wish to destruct it and free up its memory.
Internally, we track things in a structure that can result in circular
references, and without calling 'C<release()>' these will not properly get
cleaned up by Perl.  Once you've called this method, though, don't expect to be
able to do anything else with the C<Font::OTF::Font> object; it'll have B<no>
internal state whatsoever.

B<Developer note:> As part of the brute-force cleanup done here, this method
will throw a warning message whenever unexpected key values are found within
the C<Font::OTF::Font> object.  This is done to help ensure that any unexpected
and unfreed values are brought to your attention so that you can bug us to keep
the module updated properly; otherwise the potential for memory leaks due to
dangling circular references will exist.

=cut

sub release {
    my ($self) = @_;

# delete stuff that we know we can, here

    my @tofree = map { delete $self->{$_} } keys %{$self};

    while (my $item = shift @tofree) {
        my $ref = ref($item);
        if (UNIVERSAL::can($item, 'release')) { $item->release(); }
        elsif ($ref eq 'ARRAY') { push( @tofree, @{$item} ); }
        elsif (UNIVERSAL::isa($ref, 'HASH')) { release($item); }
    }

# check that everything has gone - it better had!
    for my $key (keys %{$self}) {
        warn ref($self) . " still has '$key' key left after release.\n";
    }
}

1;

__END__

=head1 BUGS

Bugs abound aplenty I am sure. There is a lot of code here and plenty of scope.
The parts of the code which haven't been implemented yet are:

=over 4

=item Post

Version 4 format types are not supported yet.

=item Cmap

Format type 2 (MBCS) has not been implemented yet and therefore may cause
somewhat spurious results for this table type.

=item Kern

Only type 0 & type 2 tables are supported (type 1 & type 3 yet to come).

=item TTC and WOFF

The current Font::OTF::Font::out method does not support the writing of TrueType
Collections or WOFF files.

=item DSIG

Haven't figured out how to correctly calculate and output digital signature (DSIG) table

=back

In addition there are weaknesses or features of this module library

=over 4

=item *

There is very little (or no) error reporting. This means that if you have
garbled data or garbled data structures, then you are liable to generate duff
fonts.

=item *

The exposing of the internal data structures everywhere means that doing
radical re-structuring is almost impossible. But it stop the code from becoming
ridiculously large.

=back

Apart from these, I try to keep the code in a state of "no known bugs", which
given the amount of testing this code has had, is not a guarantee of high
quality, yet.

For more details see the appropriate class files.

=head1 AUTHOR

Martin Hosken L<http://scripts.sil.org/FontUtils>.


=head1 LICENSING

Copyright (c) 1998-2016, SIL International (http://www.sil.org)

This module is released under the terms of the Artistic License 2.0.
For details, see the full text of the license in the file LICENSE.



=cut

