import javax.sound.sampled.Line;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.io.FileWriter;

public class Vaccine{
    private static class Tup{
      public int index, comp;
      public int[] positions;
      public String solution, prevMov;
      public Tup(int index, String solution, int comp, String prevMov, int[] positions) {
        this.index = index;
        this.solution = solution;
        this.comp = comp;
        this.prevMov = prevMov;
        //this.counter = counter;
        this.positions = positions;       
      }
    }

    public static void main(String[] args){
        try(BufferedReader br = new BufferedReader(new FileReader(args[0]))) {
          String line = br.readLine();
          int Q = Integer.parseInt(line);
          //FileWriter myWriter = new FileWriter("filename.txt");
          for(int i=0; i<Q; i++) {
            line = br.readLine();
            String[] arr = line.split(" ");
            String test=solver(arr[0]);
            System.out.print(test+"\n");
            //myWriter.write(test+"\n");
            }
            //myWriter.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }


  private static String findcomplement(String str) {
    char[] array= str.toCharArray();
    for (int i=0;i< array.length;i++) {
      if (array[i]=='G') array[i]='C';
      else if (array[i]=='C') array[i]='G';
      else if (array[i]=='A') array[i]='U';
      else if (array[i]=='U') array[i]='A';
    }
    String result = new String(array);
    return result;
  }

  public static String reverseIt(String source) {
     int i, len = source.length();
     StringBuilder dest = new StringBuilder(len);

     for (i = (len - 1); i >= 0; i--){
         dest.append(source.charAt(i));
     }

     return dest.toString();
 }

  private static String solver(String str) {
      char[] nucleus = str.toCharArray();
      int ind = nucleus.length, complements;
      char curr = nucleus[ind-1];
      String cmpltemp = findcomplement(str);
      char[] cmpl = cmpltemp.toCharArray();
      String prevMov = "p";
      Queue<Tup> q = new LinkedList<>();
      int[] pos = {0, 0, 0, 0};
      pos = push(curr, pos);
      q.add(new Tup(ind-1, prevMov, 0, prevMov, pos));
      while (!q.isEmpty()) {
        Tup tup = q.remove();
        if (tup.prevMov == "p") {
          if (tup.comp == 0) curr = nucleus[tup.index-1];
          else curr = cmpl[tup.index-1];
          complements = complement(curr, tup.positions, tup.comp);
          if (complements != tup.comp) q.add(new Tup(tup.index, tup.solution+"c", complements, "c" , tup.positions));
          pos = push(curr, tup.positions);
          if (pos != null) {
            if (tup.index-1 == 0) return tup.solution+"p";
            q.add(new Tup(tup.index-1, tup.solution+"p", tup.comp, "p" , pos));
          }
          pos = reverse(curr, tup.positions);
          if (pos != null) q.add(new Tup(tup.index, tup.solution+"r", tup.comp, "r" , pos));
        }
        else if (tup.prevMov == "c") {
          if (tup.comp == 0) curr = nucleus[tup.index-1];
          else curr = cmpl[tup.index-1];
          pos = push(curr, tup.positions);
          if (pos != null) {
            if (tup.index-1 == 0) return tup.solution+"p";
            q.add(new Tup(tup.index-1, tup.solution+"p", tup.comp, "p" , pos));
          }          
          pos = reverse(curr, tup.positions);
          if (pos != null) q.add(new Tup(tup.index, tup.solution+"r", tup.comp, "r" , pos));
        }
        else {
          if (tup.comp == 0) curr = nucleus[tup.index-1];
          else curr = cmpl[tup.index-1];
          pos = push(curr, tup.positions);
          if (pos != null) {
            if (tup.index-1 == 0) return tup.solution+"p";
            q.add(new Tup(tup.index-1, tup.solution+"p", tup.comp, "p" , pos));
          }
        }
      }
      return "1";
  }
  private static int[] push(char c, int[] positions) {
    if (c == 'A' && (positions[0] == 0 || positions[0] == 1)) {
      //System.out.println("in A");
      if (positions[1] == 0 && positions[2] == 0 && positions[3] == 0) return new int[] {1, 0, 0, 0};
      else if (positions[1] == 1 && positions[2] == 0 && positions[3] == 0) return new int[]{1, 4, 0, 0};
      else if (positions[2] == 1 && positions[1] == 0 && positions[3] == 0) return new int[] {1, 0, 4, 0};
      else if (positions[3] == 1 && positions[1] == 0 && positions[2] == 0) return new int[] {1, 0, 0, 4};
      else if (positions[1] == 1) return new int[] {1, 2, positions[2], positions[3]};
      else if (positions[2] == 1) return new int[] {1, positions[1], 2, positions[3]};
      else if (positions[3] == 1) return new int[] {1, positions[1], positions[2], 2};
      else return new int[] {1, positions[1], positions[2], positions[3]};
    }
    else if (c == 'G' && (positions[1] == 0 || positions[1] == 1)) {
      //System.out.println("in G");
      if (positions[0] == 0 && positions[2] == 0 && positions[3] == 0) return new int[] {0, 1, 0, 0};
      else if (positions[0] == 1 && positions[2] == 0 && positions[3] == 0) return new int[] {4, 1, 0, 0};
      else if (positions[2] == 1 && positions[0] == 0 && positions[3] == 0) return new int[] {0, 1, 4, 0};
      else if (positions[3] == 1 && positions[0] == 0 && positions[2] == 0) return new int[] {0, 1, 0, 4};
      else if (positions[0] == 1) return new int[] {2, 1, positions[2], positions[3]};
      else if (positions[2] == 1) return new int[] {positions[0], 1, 2, positions[3]};
      else if (positions[3] == 1) return new int[] {positions[0], 1, positions[2], 2};
      else return new int[] {positions[0], 1, positions[2], positions[3]};
    }
    else if (c == 'C' && (positions[2] == 0 || positions[2] == 1)) {
      //System.out.println("in C");
      if (positions[1] == 0 && positions[0] == 0 && positions[3] == 0) return new int[] {0, 0, 1, 0};
      else if (positions[0] == 1 && positions[1] == 0 && positions[3] == 0) return new int[] {4, 0, 1, 0};
      else if (positions[1] == 1 && positions[0] == 0 && positions[3] == 0) return new int[] {0, 4, 1, 0};
      else if (positions[3] == 1 && positions[1] == 0 && positions[0] == 0) return new int[] {0, 0, 1, 4};
      else if (positions[0] == 1) return new int[] {2, positions[1], 1, positions[3]};
      else if (positions[1] == 1) return new int[] {positions[0], 2, 1, positions[3]};
      else if (positions[3] == 1) return new int[] {positions[0], positions[1], 1, 2};
      else return new int[] {positions[0], positions[1], 1, positions[3]};
    }
    else if (c == 'U' && (positions[3] == 0 || positions[3] == 1)) {
      //System.out.println("in U");
      if (positions[1] == 0 && positions[2] == 0 && positions[0] == 0) return new int[] {0, 0, 0, 1};
      else if (positions[0] == 1 && positions[1] == 0 && positions[2] == 0) return new int[] {4, 0, 0, 1};
      else if (positions[1] == 1 && positions[2] == 0 && positions[0] == 0) return new int[] {0, 4, 0, 1};
      else if (positions[2] == 1 && positions[1] == 0 && positions[0] == 0) return new int[] {0, 0, 4, 1};
      else if (positions[0] == 1) return new int[] {2, positions[1], positions[2], 1};
      else if (positions[1] == 1) return new int[] {positions[0], 2, positions[2], 1};
      else if (positions[2] == 1) return new int[] {positions[0], positions[1], 2, 1};
      else return new int[] {positions[0], positions[1], positions[2], 1};
    }
    else return null;
  }
  private static int complement(char c, int[] positions, int compl) {
    if (c == 'A' && positions[3] != 2) return ((compl+1)%2);
    else if (c == 'G' && positions[2] != 2) return ((compl+1)%2);
    else if (c == 'C' && positions[1] != 2) return ((compl+1)%2);
    else if (c == 'U' && positions[0] != 2) return ((compl+1)%2);
    else return compl;
  }
  private static int revChar(int position) {
    if (position == 1) return 4;
    else if (position == 4) return 1;
    else return position;
  }
  private static int[] reverse(char c, int[] positions) {
    if (positions[0] == 1 && positions[1] == 0 && positions[2] == 0 && positions[3] == 0) return new int[] {1,0,0,0};
    else if (positions[1] == 1 && positions[0] == 0 && positions[2] == 0 && positions[3] == 0) return new int[] {0,1,0,0};
    else if (positions[2] == 1 && positions[0] == 0 && positions[1] == 0 && positions[3] == 0) return new int[] {0,0,1,0};
    else if (positions[3] == 1 && positions[0] == 0 && positions[1] == 0 && positions[2] == 0) return new int[] {0,0,0,1};
    else if (c == 'A' && (positions[0] == 0 || positions[0] == 4)) return new int[] {revChar(positions[0]), revChar(positions[1]), revChar(positions[2]), revChar(positions[3])};
    else if (c == 'G' && (positions[1] == 0 || positions[1] == 4)) return new int[] {revChar(positions[0]), revChar(positions[1]), revChar(positions[2]), revChar(positions[3])};
    else if (c == 'C' && (positions[2] == 0 || positions[2] == 4)) return new int[] {revChar(positions[0]), revChar(positions[1]), revChar(positions[2]), revChar(positions[3])};
    else if (c == 'U' && (positions[3] == 0 || positions[3] == 4)) return new int[] {revChar(positions[0]), revChar(positions[1]), revChar(positions[2]), revChar(positions[3])};
    else return null;
  }
}