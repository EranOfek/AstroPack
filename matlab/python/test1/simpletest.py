# python_test.py

def process_data(input_string, input_scalar, input_list):

    print('process_data started')
    
    # Process the string
    processed_string = input_string + " from Python"
    
    # Increment the scalar
    processed_scalar = input_scalar + 1
    
    # Square the numbers in the list
    processed_list = [x**2 for x in input_list]
    
    return processed_string, processed_scalar, processed_list


