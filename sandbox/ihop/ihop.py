from numpy import arange as _arange, \
                  array as _array, \
                  repeat as _repeat, \
                  datetime_as_string as _datetime_as_string, \
                  float64 as _float64, \
                  abs as _abs, \
                  ones_like as _ones_like
from xarray import Dataset as _Dataset

def makeIHOPobs(tau, ang, rcvr, ts, date_times):
    # Inputs:
    #   tau: list of observed travel times in seconds
    #   ang: list of observed arrival angles in degrees
    #   rcvr: list of receiver position in [lon, lat, depth]
    #   ts: list of timesteps
    #   date_times: list of datetime64[s] dates

    # IHOPobs dimensions
    iOBS = _arange(len(tau))

    # IHOPobs description
    my_descr   = _array('iHOP tau + ang', dtype='|S30')
    ihop_descr = _repeat( my_descr, len(iOBS) )

    # IHOPobs times
    ihop_times = _datetime_as_string(date_times, unit='s').astype(str)

    ## Slice for 'YYYYMMDD' format, and remove hyphens
    ihop_YYYYMMDD = _array([time.split('T')[0].replace('-', '') \
                         for time in ihop_times], dtype=_float64)
    ## Extract the time part for 'HHMMSS' format
    ihop_HHMMSS = _array( [time.split('T')[1].replace(':', '') \
                         for time in ihop_times], dtype=_float64 )

    # IHOPobs positions
    ihop_np= _repeat( 2.0, len(iOBS) )
    ihop_x = _repeat( rcvr[0], len(iOBS) )
    ihop_y = _repeat( rcvr[1], len(iOBS) )
    ihop_z = _repeat( _abs(rcvr[2]).astype(_float64), len(iOBS) )

    # IHOPobs values
    ihop_tau = _array(tau)
    ihop_ang = _array(ang)
    ihop_uncert = _ones_like(ihop_tau)

    # Assemble DataSet
    IHOPobs = _Dataset(
        data_vars=dict(
            ihop_descr=(['iOBS'], ihop_descr),
            ihop_date=(['iOBS'],  ihop_times),
            ihop_YYYYMMDD=(['iOBS'],ihop_YYYYMMDD),
            ihop_HHMMSS=(['iOBS'],ihop_HHMMSS),
            ihop_np=(['iOBS'],ihop_np),
            ihop_x=(['iOBS'],ihop_x),
            ihop_y=(['iOBS'],ihop_y),
            ihop_z=(['iOBS'],ihop_z),
            ihop_tau=(['iOBS'],ihop_tau),
            ihop_ang=(['iOBS'],ihop_ang),
            ihop_uncert=(['iOBS'],ihop_uncert),
            ihop_iter=(['iOBS'],_array(ts, dtype=_float64)),
    ))

    ## Add attributes
    IHOPobs['ihop_x'].attrs['units'] = 'degrees_east'
    IHOPobs['ihop_x'].attrs['standard_name']='longitude'
    IHOPobs['ihop_x'].attrs['description'] = 'Receiver longitude position'
    IHOPobs['ihop_y'].attrs['units'] = 'degrees_north'
    IHOPobs['ihop_y'].attrs['standard_name']='latitude'
    IHOPobs['ihop_y'].attrs['description'] = 'Receiver latitude position'
    IHOPobs['ihop_z'].attrs['units'] = 'm'
    IHOPobs['ihop_z'].attrs['standard_name']='depth'
    IHOPobs['ihop_x'].attrs['positive']='down'
    IHOPobs['ihop_z'].attrs['description'] = 'Receiver depth position'
    IHOPobs['ihop_tau'].attrs['units'] = 'scnds' # seconds confuses xarray plotting
    IHOPobs['ihop_tau'].attrs['standard_name']='travel tim'
    IHOPobs['ihop_tau'].attrs['description'] = 'Observed travel tim'
    IHOPobs['ihop_ang'].attrs['units'] = 'degrees'
    IHOPobs['ihop_ang'].attrs['standard_name']='arrival angle'
    IHOPobs['ihop_ang'].attrs['description'] = 'Observed arrival angles'

    # display( IHOPobs )

    return IHOPobs

def get_prt_data (file_path, keyword, nranges=0):
    tmp = file_path + '.tmp'
    _remove_PIDTID(file_path, tmp)

    try:
        match keyword:
            case 'ranges' | 'Ranges':
                header="Profile ranges (km):"
            case 'depths' | 'ssp' | 'SSP' | 'ssp+depths':
                header="Depth (m)     Soundspeed (m/s)"
            case _:
                print (keyword, ': not a header')

        arr = _extract_numbers(tmp, header)
        if nranges != 0:
            arr = arr.reshape((len(arr)//(nranges+1)), nranges+1)
            match keyword:
                case 'depths':
                    return arr[:,0]
                case 'ssp' | 'SSP':
                    return arr[:,1:]
                case _:
                    return arr
        else:   # ranges
            return arr

    except ValueError:
        return None


def _extract_numbers(file_path, headerkey):
    numbers = []  # This list will store all the numbers found after the headerkey
    start_collecting = False  # Flag to start collecting numbers after headerkey is found

    with open(file_path, 'r') as file:
        for line in file:
            # Check if we've found the headerkey
            if headerkey in line:
                start_collecting = True
                continue  # Skip the headerkey line

            # If we are after the headerkey line, start processing for numbers
            if start_collecting:
                # Try converting the line into numbers, stop reading if it's not a number
                try:
                    # Extract numbers from the current line
                    line_numbers = [float(num) for num in line.split()]
                    if line_numbers:  # If the list is not empty
                        numbers.extend(line_numbers)
                    else:
                        # If an empty line or non-numeric line is encountered
                        break
                except ValueError:
                    # Non-numeric data encountered, stop reading
                    break

    # Convert the list of numbers to a NumPy array
    return array(numbers)


def _remove_PIDTID(input_file_path, output_file_path, num_characters_to_remove=19):
    with open(input_file_path, 'r') as input_file, open(output_file_path, 'w') as output_file:
        for line in input_file:
            # Check if the line has at least 'num_characters_to_remove' characters
            if len(line) > num_characters_to_remove:
                # Write the line without the first 'num_characters_to_remove' characters
                output_file.write(line[num_characters_to_remove:])
            else:
                # If the line is shorter than 'num_characters_to_remove', write an empty line
                # or do not write anything if you wish to exclude short lines
                output_file.write('\n')
    return None
