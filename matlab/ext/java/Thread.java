// https://undocumentedmatlab.com/articles/explicit-multi-threading-in-matlab-part1

import java.io.DataOutputStream;
import java.io.FileOutputStream;
public class MyJavaThread extends Thread
{
    String filename;
    double[] doubleData;
    public MyJavaThread(String filename, double[] data)
    {
        this.filename = filename;
        this.doubleData = data;
    }
    @Override
    public void run()
    {
        try
        {
            DataOutputStream out = new DataOutputStream(
                                     new FileOutputStream(filename));
            for (int i=0; i < doubleData.length; i++)
            {
                out.writeDouble(doubleData[i]);
            }
            out.close();
        } catch (Exception ex) {
            System.out.println(ex.toString());
        }
    }
}

