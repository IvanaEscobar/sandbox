from numpy import array

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
