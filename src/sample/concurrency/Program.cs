using System;
using System.Threading;

namespace concurrency
{
    class Program
    {
        static int x = 0;
        static object lockee = 1;

        static void Main(string[] args)
        {
            RunLock();
        }

        static void RunSimple()
        {            
            const int repeat = 100;
            const int waitMs = 10;
            var thread1 = new Thread(new ThreadStart(() => {
                for (int i = 0; i < repeat; i++) {
                    Thread.Sleep(waitMs);
                    ++x;
                }
            }));
            
            var thread2 = new Thread(new ThreadStart(() => {
                for (int i = 0; i < repeat; i++) {
                    Thread.Sleep(waitMs);
                    --x;
                }
            }));

            thread1.Start();
            thread2.Start();

            thread1.Join();
            thread2.Join();

            Console.WriteLine(x);
        }

        static void RunLock() 
        {
            const int repeat = 100;
            const int waitMs = 10;
            var thread1 = new Thread(new ThreadStart(() => {
                for (int i = 0; i < repeat; i++) {
                    Thread.Sleep(waitMs);
                    lock(lockee) 
                    {                        
                        ++x;
                    }
                }
            }));
            
            var thread2 = new Thread(new ThreadStart(() => {
                for (int i = 0; i < repeat; i++) {
                    Thread.Sleep(waitMs);
                    lock(lockee) 
                    {                        
                        --x;
                    }
                }
            }));

            thread1.Start();
            thread2.Start();

            thread1.Join();
            thread2.Join();

            Console.WriteLine(x);
        }
    }
}
