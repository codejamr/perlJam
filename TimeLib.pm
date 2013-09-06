#!c:\perl\bin\perl.exe

use Exporter;
package TimeLib;
use vars qw(@ISA @EXPORT @EXPORT_OK);
@ISA    = qw( Exporter );
@EXPORT = qw( yyyymmdd_hhmmss
              yyyymmddhhmmss
              yyyymmdd
              yymmdd
              yymmdd_hhmmss
              mmdd
              mmddyy
              hhmmss
              hhmm
              hh
              yyyy_mm_dd
              hh_mm_ss
              hh_mm
              interval
            );

sub yyyy_mm_dd
{ ($s,@t) = @_;
  $s = '_'       if (!$s);
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%04d%s%02d%s%02d'
  , $t[5] + 1900
  , $s
  , $t[4] + 1
  , $s
  , $t[3] + 0
  );
}

sub hh_mm_ss
{ ($s,@t) = @_;
  $s = '_'       if (!$s);
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%02d%s%02d%s%02d'
  , $t[2] + 0
  , $s
  , $t[1] + 0
  , $s
  , $t[0] + 0
  );
}

sub hh_mm
{ ($s,@t) = @_;
  $s = '_'       if (!$s);
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%02d%s%02d'
  , $t[2] + 0
  , $s
  , $t[1] + 0
  );
}

sub yyyymmdd_hhmmss
{ ($s,@t) = @_;
  $s = '_'       if (!$s);
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%04d%02d%02d%s%02d%02d%02d'
  , $t[5] + 1900
  , $t[4] + 1
  , $t[3] + 0
  , $s
  , $t[2] + 0
  , $t[1] + 0
  , $t[0] + 0
  );
}

sub yyyymmddhhmmss
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%04d%02d%02d%02d%02d%02d'
  , $t[5] + 1900
  , $t[4] + 1
  , $t[3] + 0
  , $t[2] + 0
  , $t[1] + 0
  , $t[0] + 0
  );
}

sub yyyymmdd
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%04d%02d%02d'
  , $t[5] + 1900
  , $t[4] + 1
  , $t[3] + 0
  );
}

sub yymmdd
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%02d%02d%02d'
  , $t[5] - 100 # this is bad, but... I should be dead by the time it's a problem
  , $t[4] + 1
  , $t[3] + 0
  );
}

sub yymmdd_hhmmss
{ ($s,@t) = @_;
  $s = '_'       if (!$s);
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%02d%02d%02d%s%02d%02d%02d'
  , $t[5] - 100 # this is bad, but... I should be dead by the time it's a problem
  , $t[4] + 1
  , $t[3] + 0
  , $s
  , $t[2] + 0
  , $t[1] + 0
  , $t[0] + 0
  );
}

sub mmdd
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%02d%02d'
  , $t[4] + 1
  , $t[3] + 0
  );
} 

sub mmddyy
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return 
  sprintf
  ( '%02d%02d%02d'
  , $t[4] + 1
  , $t[3] + 0
  , $t[5] - 100 # this is bad, but... I should be dead by the time it's a problem
  );
} 

sub hhmmss
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return
  sprintf
  ( '%02d%02d%02d'
  , $t[2] + 0
  , $t[1] + 0
  , $t[0] + 0
  );
}

sub hhmm
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return
  sprintf
  ( '%02d%02d'
  , $t[2] + 0
  , $t[1] + 0
  );
}

sub hh
{ @t = @_;
  @t = localtime $t[0] if ((scalar(@t))==1);
  @t = localtime if (! scalar @t);
  return
  sprintf
  ( '%02d'
  , $t[2] + 0
  );
}

sub interval
{ my ($int) = @_;
  
  my ($years  ) = ($int =~ /[^0-9]*([0-9]+)Y.*/);
  my ($months ) = ($int =~ /[^0-9]*([0-9]+)M.*/);
  my ($weeks  ) = ($int =~ /[^0-9]*([0-9]+)w.*/);
  my ($days   ) = ($int =~ /[^0-9]*([0-9]+)d.*/);
  my ($hours  ) = ($int =~ /[^0-9]*([0-9]+)h.*/);
  my ($minutes) = ($int =~ /[^0-9]*([0-9]+)m.*/);
  
  my ($seconds) = ($int =~ /^([0-9]+)$/);
     ($seconds) = ($int =~ /[^0-9]*([0-9]+)s.*/) if (!$seconds);

  $seconds = 0 if (!$seconds); 
  $minutes = 0 if (!$minutes);
  $hours   = 0 if (!$hours);
  $days    = 0 if (!$days);
  $weeks   = 0 if (!$weeks);
  $months  = 0 if (!$months);
  $years   = 0 if (!$years);
  
  my $return = ($seconds)
             + ($minutes * 60)
             + ($hours   * 60 * 60)
             + ($days    * 60 * 60 * 24)
             + ($weeks   * 60 * 60 * 24 * 7)
             + ($months  * 60 * 60 * 24 * 31)
             + ($years   * 60 * 60 * 24 * 36)
             + ($years   * 60 * 60 * 6)
             ;

  return $return;
}


1;
