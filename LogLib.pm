#!/usr/local/bin/perl 
#!c:\perl\bin\perl.exe
#
# hm... I did not know about log4perl when I started working on this

use Exporter;
package LogLib;
use vars qw(@ISA $debug @EXPORT @EXPORT_OK);
@ISA    = qw( Exporter );
@EXPORT = qw( $debug
              $except
              debug
              debug_off
              debug_store
              debug_return
              debug_restore
              info
              failed 
              success
              warning
              error
              sitescope
              catastrophe
              done
              dolog
              PutTableToFile
              GetOneTableRecordFromFile
              dftscgi
              CreateLogEntries
              getMessages
              printif
            );

@EXPORT_OK = qw( _msg ); # this is not working, will need to figure out when desired to use


require TimeLib;
use     TimeLib; 

require LWP::UserAgent;
use     LWP::UserAgent;
require HTML::Form;
use     HTML::Form;
require File::Path;
use     File::Path;

sub debug;
sub debug_off;
sub debug_require;
sub debug_restore;
sub config; 
sub GetoptLong; 

# 2006/09/25 (np) norris.x.pouhovitch at jpmchase.com 
# override the import routine so params could be passed in 
sub import 
{
  my $pkg = shift;   # package
  my @syms = ();     # symbols to import
  my @config = ();   # configuration
  my $dest = \@syms; # symbols first
  for ( @_ ) 
  { if (/^\:(c|cfg|config|configuration)/) 
    { $dest = \@config; next; } 
    push(@$dest, $_); 
  } 

  # Hide one level and call super.
  local $Exporter::ExportLevel = 1;
  push(@syms, qw(&GetOptions)) if @syms; # always export GetOptions
  $pkg->SUPER::import(@syms);

  # And configure.
  config(@config) if @config;
  GetoptLong; 
}

$doGetoptLong   = 0; 
$doneGetoptLong = 0; 
sub config  
{ my %p = (); for(@_){$p{$_}=1;} 
  $doGetoptLong = $p{'GetoptLong'} || $p{'Getopt'} || $p{getopt}; 
} 

# 2006/09/20 (np) norris.x.pouhovitch at jpmchase.com
# The begin block attempts to preload command line
# values that will aid debugging the code
# The option can be turned off by setting the following
# before calling use LogLib
@logOptions     = ();
@debugOptions   = ();
$debug          = 0;
sub GetoptLong  
{ return unless $doGetoptLong; 
  return if   $doneGetoptLong; # it only makes sense to run ot once 
  $doneGetoptLong++; 

  eval # this will run only if $ENV{noLogLibARGV} is not set!
  { 
    # Lets check if perhaps we should turn on the debuging for the Getopt::Long package itself 
    my   @optnsGetoptLong = qw(:config gnu_compat bundling pass_through);
    push @optnsGetoptLong, 'debug' if ("@ARGV"=~/-dGetopt\:\:Long/);
   
    # use Getop::Long with all the set params
    require Getopt::Long
    &&
    import  Getopt::Long  @optnsGetoptLong
    ;

    # 2006/09/25 (np) norris.x.pouhovitch at jpmcahse.com
    # The following should really be moved to Getopt::Long package
    # as an option, but for now we will hand code it 
    my @argv = @ARGV; 

    # Do not even attempt to process the options
    # if the version is less then what I have asked for
    # The processing however will continue,
    # only the initialization of command line parameters 
    # will be aborted
    if ($Getopt::Long::VERSION < 2.35)
    { warn "WARNING: LogLib::BEGIN->Getopt::Long::VERSION = $Getopt::Long::VERSION\n";
      die;
    }
    
    # 2006/09/20 (np) norris.x.pouhovitch at jpmchase.com
    # Not sure why, but {,} specifier is not working very well
    # so I have removed it, but eventualy we want it in
    # I will have to read the Getopt source code to figure out
    # the reason and see if we can fix the behavior
    # For the time being multiple options can be provided either
    # by seperating them with , or by using the same parameter
    # multiple times like -d fw -d END

    GetOptions
    ( 'debug|d:s' => \@debugOptions # --debug -d are optional but can have a string
    , 'log|l:s'   => \@logOptions   # --log   -l are optional but can have a string
    # , 'd:s'     => \@debugOptions # --debug -d are optional but can have a string
    # , 'l:s'     => \@logOptions   # --log   -l are optional but can have a string
    );
    @debugOptions = split(/[;, ]/,join(',',@debugOptions))
    unless (@debugOptions == 1)&&(!$debugOptions[0])
    ;
    @logOptions = split(/[;, ]/,join(',',@logOptions  ))
    unless (@logOptions == 1)&&(!$logOptions[0])
    ;
    
    $ENV{'DEBUG::debug'} = 
    $debug               = @debugOptions; # gi'me the count
    debug "turned on debug mode"; 

    foreach $option (@debugOptions)
    { next unless $option;  
      $ENV{"DEBUG::$option"} = 1;
      debug "LogLib::DEBUG::$option";
    }
    debug_off; 

    # 2006/09/20 (np) norris.x.pouhovitch at jpmchase.com
    # Possible known options as of today
    # noCatastrophe
    # noTimeStamp
    # noSubName
    # noPackage
    # Look for their meanings in the code
    foreach $option (@logOptions)
    { next unless $option; 
      $ENV{"LogLib::$option"} = 1;
      debug "LogLib::$option";
    }
    
    debug "Done ARGV = @ARGV";
    @ARGV = @argv; # restore things to back how they used to be 
    debug "Restored ARGV = @ARGV"; 
  }; 
  $debug = $ENV{"DEBUG::debug"}; 
}

# 2006/08/20 (np) norris.x.pouhovitch at jpmchase.com
# incorporated chomp and s/ calls to treat the multiline
# entries properly.  Also came up with a new way
# to report log headers, instead of reporting DEBUG    000000
# a shorter version may be usefull:
# D:00:00:00: 
# The short version has not been implemented yet
# The multiline conversion has not been populated to the rest of the 
# status calls: info, warning, error, catastrophe
# 2008-10-05 (np) norris.x.pouhovitch at jpmchase.com
# OK, so I came up with a set of formats for reporting
# Here is a quick reference for when I get ready to implement
# LONG         MEDIUM         SHORT    TINY NONE
# ------------ -------------- -------- ---- ----
# DEBUG        DBUG 14:12:55> D141255> D> 
# INFO         INFO 14:12:55> I141255> I>
# WARNING      WARN 14:12:55> W141255> W> 
# ERROR        EROR 14:12:55> E141255> E>
# SUCCESS      SUCC 14:12:55> S141255> S>
# FAILED       FAIL 14:12:55> F141255> F>
# CATASTROPHE  GONE 14:12:55> C141255> C>
# CATASTROPHE  GONE 14:12:55| C141255| C|
sub debug   
{ @p = @_; 
  my $time = hhmmss if (!$ENV{'LogLib::noTimeStamp'});
  my $i = 1;

  # 2007-01-18 (np) norris.x.pouhovitch at jpmchase.com
  # added the '','','','' to remove the warnings related to uninitialized values
  my ($pkg,$file,$line,$sub) = (caller($i),'','','','');
  while 
  ( ($sub eq '(eval)'   ) # ignore eval blocks, cand id them anyway
  ||($sub eq '(unknown)') # ignore unknown blocks too
  ||($sub =~ /__ANON__/ ) # ignore anonymous blocks (Error.pm)
  ||($sub =~ /^Error::/ ) # ignore anything from Error.pm
  )
  { ($junk,$file,$junk,$sub) = (caller(++$i),'','','','');
  }
  $sub = $pkg if (!$sub);
  my $head = "DEBUG       $time";
  $head = "D$time"     if ($ENV{'LogLib::format'} eq 'short');
  $head = "D"          if ($ENV{'LogLib::format'} eq 'tiny');
  $head = "DBUG $time" if ($ENV{'LogLib::format'} eq 'medium');
  $sub =~ s/.*?\:?([^\:]+)$/$1/ if ($ENV{'LogLib::noPackage'});
  $head.=": $sub" if ((!$ENV{'LogLib::noSubName'})&&($sub));
  
  # 2006/09/20 (np) norris.x.pouhovitch at jpmchase.com
  # It is so unfortunate, but... the line numbers are not
  # so helpfull because they rarely represent the line at which 
  # the debug call is actually made :(
  # I wish it was not so... perhaps there is something I have not
  # figured out about how this works or is intended to.
  # $head.=": $line" if ($line);
  
  foreach $ln (@p) 
  { chomp $ln;
    $ln =~ s/\n/\n$head> /g;
    my $n="\n" if(substr($ln,-1,1) ne "\n"); 
    print "$head: $ln$n" if ($debug); 
  } 
}

# 2007-01-16 (np) norris.x.pouhovitch at jpmchase.com
# This function was created to be a generic low level
# engine to perform issue reporting based on named
# categories.  It processes information display
# and accounting in a unified manner.
# this is an internal method, not meant for an export
# however, it is fairly generic, and so
# I have made it exportOK
sub _msg
{ @p = @_;
  my $time = hhmmss if (!$ENV{'LogLib::noTimeStamp'});

  # caller is anything following the last colon(:) in function name
  # if colon is not found, then whole string is it
  my ($caller) = ((caller(1))[3] =~ /(?:^|[\:])([^\:]+)$/);
  my $head     = sprintf "%-11s $time", uc($caller);
     $head     = sprintf "%-4s $time" , uc(substr($caller,0,4)) if ($ENV{'LogLib::format'} eq 'medium');
     $head     = sprintf "%-1s$time"  , uc(substr($caller,0,1)) if ($ENV{'LogLib::format'} eq 'short');
     $head     = sprintf "%-1s"       , uc(substr($caller,0,1)) if ($ENV{'LogLib::format'} eq 'tiny');
  my $msg      = undef;
  foreach $ln (@p)
  { chomp $ln;
    $ln =~ s/\n/\n$head> /g;
    my $n="\n" if(substr($ln,-1,1) ne "\n");
    $msg .= "$head: $ln$n";
  }
  print $msg;       

  eval "\$LogLib::$caller++;";
  $except++ if ($caller eq 'error'      ); # keep backwards compatibility, at least for now
  die $msg  if ($caller eq 'catastrophe'); # catastrophies are always special, and do involve death
}

sub info        {return _msg(@_);} # just some info
sub success     {return _msg(@_);} # some very good knews
sub warning     {return _msg(@_);} # no alerts, processing moves on
sub error       {return _msg(@_);} # alert, but process the rest of the files
sub failed      {return _msg(@_);} # alert and the bundle is done right here
sub catastrophe {return _msg(@_);} # you are single duck with 20 guns on your tail

# 2007-01-16 (np) norris.x.pouhovitch at jpmchase.com
# converted all these functions into a single standard function
# that sets different variables depending on which way it was called
#
#sub info 
#{ @p = @_;
#  my $time = hhmmss.' ' if (!$ENV{'LogLib::noTimeStamp'});
#  foreach $ln (@p)
#  { my $n="\n" if(substr($ln,-1,1) ne "\n");
#    print "INFO        $time: $ln$n";
#  }
#}
#
#sub success
#{ @p = @_;
#  my $time = hhmmss.' ' if (!$ENV{'LogLib::noTimeStamp'});
#  foreach $ln (@p)
#  { my $n="\n" if(substr($ln,-1,1) ne "\n");
#    print "SUCCESS     $time: $ln$n";
#  }
#}
#
#sub warning 
#{ @p = @_;
#  my $time = hhmmss.' ' if (!$ENV{'LogLib::noTimeStamp'});
#  foreach $ln (@p)
#  { my $n="\n" if(substr($ln,-1,1) ne "\n");
#    print "WARNING     $time: $ln$n";
#  }
#}
#
#sub error 
#{ @p = @_;
#  my $time = hhmmss.' ' if (!$ENV{'LogLib::noTimeStamp'});
#  foreach $ln (@p)
#  { my $n="\n" if(substr($ln,-1,1) ne "\n");
#    print "ERROR       $time: $ln$n";
#  }
#  $except++;
#}
#
#sub failed
#{ @p = @_;
#  my $time = hhmmss.' ' if (!$ENV{'LogLib::noTimeStamp'});
#  foreach $ln (@p)
#  { my $n="\n" if(substr($ln,-1,1) ne "\n");
#    print "FAILED      $time: $ln$n";
#  }
#  $except++;
#}
#
#sub catastrophe 
#{ @p = @_;
#  my $time = hhmmss.' ' if (!$ENV{'LogLib::noTimeStamp'});
#  my $catastrophe = '';
#  foreach $ln (@p)
#  { my $n="\n" if(substr($ln,-1,1) ne "\n");
#    $catastrophe .= sprintf '%s', "CATASTROPHE $time: $ln$n";
#  }
#  die $catastrophe if (!$ENV{'LogLib::noCatastrophe'});
#}

sub sitescope 
{ ($r,@p) = @_;
  print 'SITESCOPE '.yyyy_mm_dd('-').' '.hh_mm_ss(':')."\n";
  print @p;
  print "\nReturn Code: $r\n";
  exit $r;
}

#sub info    {@p = @_; foreach $ln (@p) {my $n="\n" if(substr($ln,-1,1) ne "\n");print "INFO   : $ln$n";}}
#sub warning {@p = @_; foreach $ln (@p) {my $n="\n" if(substr($ln,-1,1) ne "\n");print "WARNING: $ln$n";}}
#sub error   {@p = @_; foreach $ln (@p) {my $n="\n" if(substr($ln,-1,1) ne "\n");print "ERROR  : $ln$n";}; $except++;}
sub done    {debug "Exit code = $except\n";exit $except;}


########################################################################################
# PutHashToFile
#
# 2005/09/30 norris.x.pouhovitch@jpmchase.com
#
# The sub has two required parameters and one optional.  The first required parameter
# is a reference to a hash containing log parameters, the second required parameter is
# the full name of a file where these parameters will be written to.
#
# Note, this file will not be overwritten, but rather appended to, if this is not what
# you desire, then clear the file before making this call.
#
# The third parameter is optional and represents a set of characters that will be used
# as a separator between the name and the vaue pairs, so that data would be written into
# the file in the following fassion:
# <name><separator><value><newline>
#
# The sub randomly retrieves all the hash elements and translates the newline and
# carriage return characters into <\n> and <\r> respectively so that each name value
# pair would be considered a single line.
#
# Then it writes the name value pairs into the file one pair per line.
# 
# 2006/06/27 (np) norris.x.pouhovitch at jpmchase.com
# migrated this code from mmrget.pl into this library
# this code will be extended to store a list of hashes into a file
#
sub PutHashToFile
{
  my ($hash, $file, $split) = (@_);
  debug "PutHashToFile";

  $split = ($split) || "|";
  $file || return 0;
  $hash || return 0;

  if (open(SUCCESS,">>$file"))
  {
    foreach my $key (keys(%$hash))
    {
      if ($key)
      {
        $hash->{$key} =~ s/\n/<\\n>/gs;
        $hash->{$key} =~ s/\r/<\\r>/gs;
        print SUCCESS "$key$split".($hash->{$key}||"")."\n";
      }
    }
    close SUCCESS;
    return 1;
  }
  else
  {
    print "WARNING: I was not able to store hash metadata\n"
        . "         in the ($file) file\n"
        . "         because I failed to open it.\n"
        . "         So instead I decided to dump the data in here:\n"
        ;
    foreach my $key (keys(%$hash))
    {
      print "$key = ".$hash->{$key};
    }
  }

  debug "PutHashToFile finished.";
  return 0;
}


########################################################################################
# PutTableToFile
#
# 2005/09/30 norris.x.pouhovitch@jpmchase.com
#
# This is an extention of PutHashToFile, which adds an ability to store 
# an array of hashes in one file.  The idea is to temporarily use this for loging.
# 
local $gLogFileName   = '';
local $gLogFileHandle = '';
sub PutTableToFile
{ my ($p) = (@_);  debug_off;
  debug "PutTableToFile";

  if ($debug) 
  { debug "Parameters:";
    foreach $param (keys(%$p)) {debug "$param = '$$p{$param}'";}
    debug "";
  }

  $$p{'delim'} = $$p{'delim'}   || "|";
  $$p{'split'} = $$p{'split'}   || "<log-lib-pm-new-record>\n";
  $$p{'file'}  || $gLogFileName || $gLogFileHandle || return 0;
  $$p{'hash'}  || $$p{'list'}   || return 0;

  debug "delim = ".$$p{'delim'};
  debug "split = ".$$p{'split'};

  if ((!$gLogFileHandle) && (($gLogFileName) || ($$p{'file'})))
  { $gLogFileName = $$p{'file'} if (!$gLogFileName) || ( $gLogFileName ne $$p{'file'});
    open $gLogFileHandle, ">>$gLogFileName" || die "unable to open logfile $gLogFileName!";
    debug "opened $gLogFileName";
    
    # enable the handle we just opened to automatically flush the data to the disk
    # slower operations, but safer for log keeping, should not affect the performance that badly
    # can be disabled at any time in the future if it is causing an error, and logging could be 
    # redone to be perfomed into a queue ;)
    { my $stdout = select;
      select $gLogFileHandle;
      $| = 1;
      select $stdout;
    }
  }

  if ($$p{'hash'})
  { if ($$p{'list'})
    { push @{$$p{'list'}}, $$p{'hash'}; }
    else
    { $$p{'list'} = [$$p{'hash'}];}
  }

  debug "p{list}.count = ".scalar(@{$$p{'list'}});
  foreach $hash (@{$$p{'list'}})
  { debug "key is a reference of ".ref($hash);
    foreach my $key (keys(%$hash))
    {
      if ($key)
      {
        $$hash{$key} =~ s/\n/<\\n>/gs;
        $$hash{$key} =~ s/\r/<\\r>/gs;
        my $printdata = "$key".$$p{'delim'}.($$hash{$key}||"")."\n";
        debug $printdata;
        print $gLogFileHandle $printdata;
      }
    }
    print $gLogFileHandle $$p{'split'};
  }
  
  debug "PutTableToFile finished.";
  debug_restore;
  return 0;
}


########################################################################################
# dolog
#
# 2007/03/05 norris.x.pouhovitch at jpmchase.com
#
# This is an extention of PutTableToFile, which adds an ability to send an email
# based on the configuration.
#
# The function does change the interface on how it is called, it expects
# a reference to a hash that contains log information as well as a reference to
# a hash that describes a particular log entry.  One can send multiple references
# if there are multiple log entries to be created in one swoop.
# If an email to be sent has been requested, it will be created and sent once per
# log entry, it will basically contain the log record in somewhat nicely formated
# manner.
#
# The function will also perform some guessing as well, and so the reference to the 
# log config structure will be made optional, if it is not provided then the 
# function will go ahead and try to look it up on it's own.
#
# If the passed in parameter is a scalar, then it will assume that it is a simple
# log comment, and since we do not have a better place to store it in, will
# be shoved in to the ACTION_CMD field.
#
# Which brings me to another point... this loging structure has got to change!!!
# 
# 2007-09-07 (np) norris.x.pouhovitch at jpmchase.com 
# Fixed a bug in log file naming, too bad it took so long to detect it
# Added debugging code to the function 
require Mail::SendEasy;
use     Mail::SendEasy;
my %smtp = ();
sub dolog
{ my ($cfg,@P) = (@_); debug_off; 

  # sanity check
  die "dolog expects first parameter to be a reference to the log structure!" 
   if ((ref($cfg) ne 'HASH') || (ref($cfg->{elements}) ne 'HASH'))
  ; 

  # Make sure you process the log entry if there was nothing else passed in
  push @P, $cfg->{elements} if (!scalar(@P));

  foreach $p (@P)
  { if ((ref($p))&&(exists($p->{elements})))
    { $cfg = $p  
    }
    else
    { my 
      $logfile  = $cfg->{path} || $ENV{DFTSLOGS} || "$ENV{DFTS}/logs/";
      $logfile .= '/' unless ($logfile =~ /\/$/); # 2007-09-07 (np) fix 
      $logfile .= $cfg->{filename};
      debug "logfile($logfile)";

      %log = %{$cfg->{elements}};
      if (!ref($p))
      { $log{ACTION_CMD} = $p;
      }
      elsif (ref($p) eq 'HASH')
      { %log = (%log,%$p);
      }
      else
      { warning "dolog received an unrecognised parameter($p)"
        .       "I will log whatever default settings I have, "
        .       "but results may be less then desireble"
        ;
      }

      # 2007-09-07 (np) fix converted () to {} 
      PutTableToFile
      { 'hash' => \%log
      , 'file' => $logfile
      }; 

      if ($cfg->{email})
      { my %msg            = (%{$cfg->{email}{message}});
        $msg{subject}      = "$log{RESULT} $log{ACTION} $log{FILENAME_AFTER}";
        $msg{msg}          = '';
        $msg{to}           =~ s/ +/ /g;
        $msg{subject}      =~ s/ +/ /g;
        #while (($key,$val) = each(%log))
        @keys = sort(keys(%log));
        foreach $key (@keys)
        { $msg{msg}       .= sprintf('%15s: %s'."\n",$key,$log{$key});
	}
        
        
        if (!Mail::SendEasy::send(%{$cfg->{email}{server}},%msg))
        { # WHAT DO I DO HERE????
          # CREATE A JOB TO FAIL????
          # SPIT A WARNING OUT TO THE SYSOUT????
          warning "smtp::send has failed with:"
          ,       Mail::SendEasy::error
          ;
        }
        else
        { info "email sent";
          foreach $to ((split(/ *, */, "$msg{to} , $msg{cc} , $msg{bcc}"))) 
          {info "to($to)" if ($to);}
          info "subject($msg{subject})";
        }
      }
    }
  }
  debug_restore; 
}

########################################################################################
# GetHashFromFile
#
# 2005/09/30 norris.x.pouhovitch@jpmchase.com
#
# This sub is a reverse of PutHashToFile, it accepts same three parameters, the first
# two are the reference to the hash that name value pairs will be stored to, the second
# is the file name to read.  Both first two parameters are requred.  The third parameter
# is optional and is used to find out what character or set their of should be
# considered to be the separator between the value name pairs.
#
# The sub ignores empty lines, and performs revers conversion of <\n> and <\r> into
# new line and carriage return characters.
#
# 2006/06/27 (np) norris.x.pouhovitch at jpmchase.com
# migrated this code from mmrget.pl into this library
# this code will be extended to retrieve a list of hashes from a file
#
sub GetHashFromFile
{
  my ($hash, $file, $split) = (@_);
  debug "GetHashFromFile";

  $split = ($split) || "|";
  $split =~ s/([\|\$\[\]\(\)\{\}\.\*\+\^])/\\$1/gs; #escape regexp special characters
  $file || return 0;
  $hash || return 0;

  if (open(SUCCESS,"<$file"))
  {
    while (<SUCCESS>)
    {
      my $line = $_;
      my ($key, $value) = ($line =~ /([A-Za-z_]+)$split(.*)/);
      next unless ($key); # skip the blank lines ok

      chomp($value);
      $value =~ s/<\\n>/\n/gs if $value;
      $value =~ s/<\\r>/\r/gs if $value;
      $hash->{$key} = ($value||"");
      debug "GetHashFromFile $key = $value\n$line";
    }
    close SUCCESS;
    return 1;
  }
  else
  {
    print "WARNING: I was not able to open the ($file) file for output.\n"
        . "         Hence I could not get the hash values!\n"
        ;
  }

  debug "GetHashFromFile finished.";
  return 0;
}


use constant DEFAULT_HASH_DELIMETER   => '|';
use constant DEFAULT_RECORD_SEPARATOR => '<log-lib-pm-new-record>';

########################################################################################
# GetOneTableRecordFromFile
#
# 2006/07/03 norris.x.pouhovitch@jpmchase.com
#
# This is a modified version of GetHashFromFile function, which reads one record at 
# a time, stores the results in a new anonymous hash, and returns a reference to it
# if there was a record read, else returns undef, which is an indicator that an end
# of file has been reached.  Errors are communicated by raising die exceptions.
# 
local $gGetLogFileName   = '';
local $gGetLogFileHandle = '';
sub GetOneTableRecordFromFile
{ my ($p) = (@_);  
  my $PutTableToFileDebug = $debug; $debug = 0;
  debug "PutTableToFile";

  $$p{'delim'} = $$p{'delim'}      || DEFAULT_HASH_DELIMETER;
  $$p{'split'} = $$p{'split'}      || DEFAULT_RECORD_SEPARATOR;
  $$p{'file'}  || $gGetLogFileName || $gGetLogFileHandle || return 0;
  $$p{'filehandle'}                || $gGetLogFileHandle || return 0;
  debug "GetOneTableRecordFromFile";

  my $delim = $$p{'delim'} || DEFAULT_HASH_DELIMETER;
  my $split = $$p{'split'} || DEFAULT_RECORD_SEPARATOR;
  my $hash  = {};
  $delim =~ s/([\|\$\[\]\(\)\{\}\.\*\+\^])/\\$1/gs; #escape regexp special characters

  if ($$p{'file'})
  { open $gGetLogFileHandle
       , "<".$$p{'file'}
      or
     die "Unable to open $gGetLogFileHandle for reading!"
       ;
  }

  $gGetLogFileHandle = $$p{'filehandle'} if ($$p{'filehandle'});

  while (my $line = <$gGetLogFileHandle>)
  { return $hash if ($line =~ /^$split$/); # we are done if we ran into an end of a record
    my ($key, $value) = ($line =~ /([A-Za-z_]+)$delim(.*)/);
    next unless ($key); # skip the blank lines ok

    chomp($value);
    $value =~ s/<\\n>/\n/gs if $value;
    $value =~ s/<\\r>/\r/gs if $value;
    $hash->{$key} = ($value||"");
    debug "GetOneTableRecordFromFile $key = $value\n$line";
  }
  debug "GetOneTableRecordFromFile finished.";
  return $hash if (scalar(keys(%$hash)));
  return undef;
}


# url      - scalar
# path     - scalar
# call     - scalar
# files    - reference to a list of hashes
# params   - reference to a hash
sub dftscgi
{ my ($p) = (@_); # params should be passed as a reference to a hash
  debug "dftscgi";

  $$p{'url' } = $$p{'url'}  || $ENV{'DFTS_CONFIG_URL'} || 'http://gti6a004.svr.bankone.net';
  $$p{'path'} = $$p{'path'} || '/cgi-bin/';
  die "call is a required parameter when calling dftscgi!" if (!$$p{'call'});

  my $url    = $$p{'url'}."/".$$p{'path'}."/".$$p{'call'};
  my $frmsrc = qq|<form method="post" enctype="multipart/form-data" action="$url">|;
  foreach my $key (keys %{$$p{'params'}})
  { $frmsrc .= qq|\n<br><input type="text" name="$key" value=""/>|;
  }
  foreach my $key (keys %{$$p{'files'}})
  { $frmsrc .= qq|\n<br><input type="file" name="$key" value=""/>|;
  }
  $form_src .= "\n<br></form>";

  debug "Parsing the submit form";
  my $ua   = LWP::UserAgent->new;
  my $form = HTML::Form->parse($frmsrc,$$p{'url'});

  debug "Setting the form values from the hash";
  foreach my $key (keys %{$$p{'params'}})
  { debug "$key = ".${$$p{'params'}}{$key};
    eval{$form->param($key,(${$$p{'params'}}{$key}||"")) if ($key);};
  }
  foreach my $key (keys %{$$p{'files'}})
  { debug "$key = ".${$$p{'files'}}{$key};
    eval{$form->param($key,(${$$p{'files'}}{$key}||"")) if ($key);};
  }

  debug "Submitting the form.";
  my $resp = $ua->simple_request($form->make_request);

  debug $resp->as_string;

  debug "dftscgi before the exit.";
  return 1 if ($resp->as_string =~ /<[Hh]1>Success<\/[Hh]1>/);

  debug "dftscgi form post error: \n"
      . $resp->as_string
      ;
      
  die $resp->as_string;
}

sub CreateLogEntries
{ my ($p) = @_;
  $$p{'call'} = 'CreateLogEntries';
  return dftscgi $p;
}


# ------------------------------------------------------------------------------------------
# debug_off
# ------------------------------------------------------------------------------------------
# PURPOSE
# 
# To turn debugging off within functions, so that debug output would be minimized
# However, debugging is not turned off if the ${$ENV{DEBUG}}{function} or
# ${$ENV{DEBUG}}{'package::function'} are set to true.
#
# The main idea is that most library functions should start with debug_off
# and should end with debug_restore, this way no debug output would be produced
# for these functions unless user has explicitly specified the debug
# output of the specific function to be displayed.
#
# When turning debugging off, the former state of $debug is stored in a global
# hash %debug_off, this is so that debug value could be restored back to 
# the original value when the debug_restore is called.
# 
# The function will work without any parameters at all, however, if there are 
# any specified then it will check to see if the parameter option is numeric, and if so
# will perform the lookup based on the perl internal structures going back the number
# of frames specified in the call, if it is not numeric, it will assume that 
# the present debug value should be stored for the function names before
# resetting the original to off.
#
# The same is true for the debug_restore, except that only the very first
# option will be used, since there is no way to merge multiple restores.
# ------------------------------------------------------------------------------------------
my %debug_off;
sub debug_off
{ return if (!$ENV{'DEBUG::debug'});
  my @params = @_;
  push @params, 1 if (!scalar(@params));
  my ($pkg, $file, $line, $sub, $func, $param);
  foreach $param (@params)
  { if ($param =~ /^[0-9]+$/)
    { ($pkg, $file, $line, $sub) = caller($param);

      # 2006/09/25 (np) norris.x.pouhovitch at jpmcahse.com 
      # keep rolling untill we get out of (eval) land 
      while ((!$ENV{'DEBUG::(eval)'}) && ($sub =~ /\(eval\)/)) 
      { $param++; 
        ($pkg, $file, $line, $sub) = caller($param); 
      }

      # if the sub was not provided then fall back to the package
      $sub = $pkg if ((!$sub)&&($param=~/^[0-9]+$/)); 
    }
    else
    { $sub = $param;}
    
    my ($func) = ($sub=~/\:([^\:]+)$/);
    $debug_off{$sub} = $debug if ($sub);

    #IF YOU NEED TO DEBUG DEBUG, YOU WILL NEED TO TURN IT ON
    #info "debug_off: sub  $sub";
    #info "debug_off: pkg  $pkg";
    #info "debug_off: func $func";
    #info "debug_off: env{sub}  ".$ENV{"DEBUG::$sub"};
    #info "debug_off: env{pkg}  ".$ENV{"DEBUG::$pkg"};
    #info "debug_off: env{func} ".$ENV{"DEBUG::$func"};
    #@keys = keys %ENV;
    #info "debug_off: env: @keys";
    
    my @checks = ("DEBUG::$func","DEBUG::$pkg");
    if ($sub =~ /\:\:/)
    { $built = '';
      foreach $part (split /\:\:/, $sub)
      { next if (!$part);
        if (!$built) { $built = $part;}
        else         { $built.= "::$part";}
        push @checks, "DEBUG::$built";
      }
    }
    else
    { push @checks, "DEBUG::$sub";}

    $debug = 0;
    foreach $check (@checks)
    { $debug = $debug || $ENV{$check};
    }
  }
}

# debug_store is identical to debug_off for the excetion of one line!!!
sub debug_store
{ return if (!$ENV{'DEBUG::debug'});
  my @params = @_;
  push @params, 1 if (!scalar(@params));
  my ($pkg, $file, $line, $sub, $func, $param);
  foreach $param (@params)
  { if ($param =~ /^[0-9]+$/)
    { ($pkg, $file, $line, $sub) = caller($param);
       $sub = $pkg if ((!$sub)&&($param==1));
    }
    else
    { $sub = $param;}
    
    my ($func) = ($sub=~/\:([^\:]+)$/);
    $debug_off{$sub} = $debug if ($sub);
  # $debug = ${$ENV{DEBUG}}{$func} || ${$ENV{DEBUG}}{$sub} || ${$ENV{DEBUG}}{$pkg};
  }
}

sub debug_restore
{ return if (!$ENV{'DEBUG::debug'});
  my @params = @_;
  push @params, 1 if (!scalar(@params));
  my ($pkg, $file, $line, $sub, $func, $param);
  foreach $param (@params)
  { if ($param =~ /^[0-9]+$/)
    { ($pkg, $file, $line, $sub) = caller($param);
       $sub = $pkg if ((!$sub)&&($param==1));
    }
    else
    { $sub = $param;}
    
    if ((!exists($debug_off{$sub}))||(!$sub))
    { $debug = $ENV{'DEBUG::debug'}; }
    else
    { $debug = $debug_off{$sub}; }    

    #IF YOU NEED TO DEBUG DEBUG, YOU WILL NEED TO TURN IT ON
    #info "debug_restore: $sub = $debug_off{$sub}";
  }
}

sub debug_return
{ return if (!$ENV{'DEBUG::debug'});
  my @params = @_;
  foreach $param (@params)
  { debug_restore $param;
    debug_off     $param;
  }
}

sub getMessages
{ my @params = @_;
  my @list   = ();
  foreach $param (@params)
  { if    (ref($param) eq 'ARRAY') {push @list, @$param;   } # add each item from the array
    elsif (ref($param) eq 'HASH' ) {push @list, (%$param); } # treat a hash as an array
    else                           {push @list, $param;    } # scalars go right in
  }

  my $count = scalar(@list); return if (!$count);
  
  my $return = "We have found ($count) messages:\n\n\n";
  for (my $i; $i<$count; $i++)
  { $n = $i + 1;
    $return .= "MESSAGE #$n\n$list[$i]\n\n";
  }

  return $return;
}


;
# function printif
# produce output depending on what $to was set to
# if an existing file, then append to the file
# if empty, then print to stdout
# if not empty yet not a file then create a directory
# and print to each file individualy
sub printif
{ my 
  ( $to
  , $table
  , $header
  , $text
  , $time
  , $action
  ) 
  = @_
; my $linebreak = '-'x80

; return unless ($to)
; return unless (ref($to) eq 'ARRAY')
; return unless (scalar(@$to))
; $text = $$text if (ref($text) eq 'SCALAR')
; return unless ($text)
; return if ($text =~ /^[\n\s\r]*$/)
; $text =~ s/^[\n\r]+//g
; chomp $text
; mkpath $$to[0] unless ((-e $$to[0])||($$to[0] =~ /^[0-9]+$/))
; if 
  ( -f $$to[0]
  )
  { unless (open FILE, ">>$$to[0]")
    { print "\n$linebreak"
    ; print "\n$header($table): - unable to topen existing file($$to[0])"
    ; print "\n$linebreak"
    ; print "\n$text\n"
    }
  ; print FILE "\n$linebreak"
  ; print FILE "\n$header($table):"
  ; print FILE "\n$linebreak"
  ; print FILE "\n$text\n"
  ; close FILE
  }
  elsif 
  ( -d $$to[0]
  )
  { my 
    $file  = "$$to[0]/$table"
  ; $file .= ".$time"   if ($time  )
  ; $file .= ".$action" if ($action)
  ; $file .= ".$$.$header.txt"
  ; unless (open FILE, ">$file")
    { print "\n$linebreak"
    ; print "\n$header($table): - unable to topen individual file($file)"
    ; print "\n$linebreak"
    ; print "\n$text\n"
    }
  ; print FILE "$text\n"
  ; close FILE
  }
  else
  { print "\n$linebreak"
  ; print "\n$header($table):"
  ; print " - unable to create path($$to[0])" unless ($$to[0] =~ /^[0-9]+$/)
  ; print "\n$linebreak"
  ; print "\n$text\n"
  }
}



1;
