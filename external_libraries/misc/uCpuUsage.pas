{
http://w-shadow.com/blog/2006/08/27/how-to-get-the-cpu-usage-of-a-process/
(c) Janis Elsts, http://w-shadow.com/
Last Updated : 27.08.2006

uCpuUsage.pas provides some functions that let you
get the CPU usage (in percent) of a given process. Note that
the usage is calculated for a *period of time* elapsed since
last wsCreateUsageCounter or wsGetCpuUsage call for that process.
This unit is freeware, feel free to use/modify in any way you like.

Using this unit :

  cnt : PCPUUsageData;
  ....
  //Initialize the counter
  cnt:=wsCreateUsageCounter(Process_id);
  //Allow for some time to elapse
  Sleep(500);
  //Get the CPU usage
  usage:=wsGetCpuUsage(cnt);
  //The returned value is a real number between 0 and 100 (representint %).
  //Destroy the counter and free memory
  wsDestroyUsageCounter(cnt);
}

// modified by detlef oertel uib gmbh / opsi.org for use with lazarus in the opsi project

unit uCpuUsage;

{$mode delphi}

interface

const
 wsMinMeasurementInterval=250; {minimum amount of time that must
 have elapsed to calculate CPU usage, miliseconds. If time elapsed
 is less than this, previous result is returned, or zero, if there
 is no previous result.}
 
type
  TCPUUsageData=record
    PID,Handle:dword;
    oldUser,oldKernel:Int64;
    LastUpdateTime:dword;
    LastUsage:single; //Last result of wsGetCpuUsage is saved here
    Tag:dword; //Use it for anythin you like, not modified by this unit
  end;
  PCPUUsageData=^TCPUUsageData;

function wsCreateUsageCounter(PID:dword):PCPUUsageData;
function wsGetCpuUsage(aCounter:PCPUUsageData):single;
procedure wsDestroyUsageCounter(aCounter:PCPUUsageData);

implementation

uses Windows,jwawinbase;

function wsCreateUsageCounter(PID:dword):PCPUUsageData;
var
 p:PCPUUsageData;
 mCreationTime,mExitTime,mKernelTime,
 mUserTime:_FILETIME;
 h:dword;
begin
 result:=nil;
 //We need a handle with PROCESS_QUERY_INFORMATION privileges
 h:=OpenProcess(PROCESS_ALL_ACCESS,false,PID);
 if h=0 then exit;
 new(p);
 p.PID:=PID;
 p.Handle:=h;
 p.LastUpdateTime:=GetTickCount;
 p.LastUsage:=0;
 if GetProcessTimes(p.Handle,mCreationTime,mExitTime,mKernelTime,
                 mUserTime) then begin
  //convert _FILETIME to Int64               
  p.oldKernel:=int64(mKernelTime.dwLowDateTime or
          (mKernelTime.dwHighDateTime shr 32));
  p.oldUser:=int64(mUserTime.dwLowDateTime or
          (mUserTime.dwHighDateTime shr 32));
  Result:=p;
 end else begin
  dispose(p);
 end;
end;

procedure wsDestroyUsageCounter(aCounter:PCPUUsageData);
begin
 CloseHandle(aCounter.Handle);
 dispose(aCounter);
end;

function wsGetCpuUsage(aCounter:PCPUUsageData):single;
var
 mCreationTime,mExitTime,mKernelTime,
 mUserTime:_FILETIME;
 DeltaMs,ThisTime:cardinal;
 mKernel,mUser,mDelta:int64;
begin
 result:=aCounter.LastUsage;

 ThisTime:=GetTickCount;
 //Get the time elapsed since last query
 DeltaMs:=ThisTime-aCounter.LastUpdateTime;
 if DeltaMs<wsMinMeasurementInterval then exit;
 aCounter.LastUpdateTime:=ThisTime;

 GetProcessTimes(aCounter.Handle,mCreationTime,mExitTime,mKernelTime,
                 mUserTime);

 //convert _FILETIME to Int64
 mKernel:=int64(mKernelTime.dwLowDateTime or
          (mKernelTime.dwHighDateTime shr 32));
 mUser:=int64(mUserTime.dwLowDateTime or
          (mUserTime.dwHighDateTime shr 32));

 //get the delta
 mDelta:=mUser+mKernel-aCounter.oldUser-aCounter.oldKernel;
 aCounter.oldUser:=mUser;
 aCounter.oldKernel:=mKernel;
 Assert(DeltaMs>0);
 Result:=(mDelta/DeltaMs)/100; //mDelta is in units of 100 nanoseconds, so...
 aCounter.LastUsage:=Result; //just in case you want to use it later, too
end;

end.
