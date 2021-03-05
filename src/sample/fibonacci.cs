using System;
using System.Linq;
using System.Collections.Generic;
					
public class Program
{
	public static void Main()
	{
		IEnumerable<int> fibo = Enumerable.Empty<int>();
		fibo = new [] { 0, 1 }.ConcatLazy(new Lazy<IEnumerable<int>>(() => fibo));
		
		Console.WriteLine(string.Join(",", fibo.Take(6)));
	}
}

public static class Ext
{
	public static IEnumerable<T> ConcatLazy<T>(this IEnumerable<T> @this, Lazy<IEnumerable<T>> v) 
	{
		foreach (var t in @this) yield return t;
		foreach (var t in v.Value) yield return t;
	}
}