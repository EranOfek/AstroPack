// https://stackoverflow.com/questions/19936483/c-reading-csv-file

void readCSV(const string &strPath2Dataset)
{   
    ifstream csvFile;
    string strPathCSVFile = strPath2Dataset + "/test.csv";
    csvFile.open(strPathCSVFile.c_str());

    if (!csvFile.is_open())
    {
        cout << "Path Wrong!!!!" << endl;
        exit(EXIT_FAILURE);
    }

    vector<long double> timeStampIMU;
    vector<long double> gyro_X;
    vector<long double> gyro_Y;
    vector<long double> gyro_Z;

    vector<long double> acc_X;
    vector<long double> acc_Y;
    vector<long double> acc_Z;

    string line;
    vector <string> vec;
    getline(csvFile, line); // skip the 1st line

    while (getline(csvFile,line))
    {
        if (line.empty()) // skip empty lines:
        {
            //cout << "empty line!" << endl;
            continue;
        }

        istringstream iss(line);
        string lineStream;
        string::size_type sz;

        vector <long double> row;

        while (getline(iss, lineStream, ','))
        {  
            row.push_back(stold(lineStream,&sz)); // convert to double
        }

        timeStampIMU.push_back(row[0]);

        gyro_X.push_back(row[1]);
        gyro_Y.push_back(row[2]);
        gyro_Z.push_back(row[3]);

        acc_X.push_back(row[4]);
        acc_Y.push_back(row[5]);
        acc_Z.push_back(row[6]);
    }

    //cout << "size ts = " << timeStampIMU.size() << endl;
    for (size_t i = 0; i < timeStampIMU.size(); i++)
    {
        cout << "ts_imu = " << setprecision(12) << timeStampIMU[i] << endl;

        cout << "gx = " << setprecision(12) << gyro_X[i] << endl;
        cout << "gy = " << setprecision(12) << gyro_Y[i] << endl;
        cout << "gz = " << setprecision(12) << gyro_Z[i] << endl;

        cout << "ax = " << setprecision(12) << acc_X[i] << endl;
        cout << "ay = " << setprecision(12) << acc_Y[i] << endl;
        cout << "az = " << setprecision(12) << acc_Z[i] << endl;
        cout << "--------------------------------" << endl;
    }
}

