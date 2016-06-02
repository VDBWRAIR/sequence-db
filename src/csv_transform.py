from toolz.dicttoolz import assoc, dissoc, merge
from toolz.itertoolz import mapcat
from Bio import SeqIO
from glob import glob
import csv 
from dateutil.parser import parse
from fn import F, _
import sys
import os 
log = sys.stderr.write

def find(pred, seq):
    res = filter(pred, seq)
    return None if not res else res[0]

segments = ["HA", "MP", "NA", "NP", "NS", "PA", "PB1", "PB2"]
#SEGMENT_DIR = "10454"
#seg_seqs = map(seq_for_segment, segments)
#return map(lambda seg,seq: assoc(new_d, seg, seq), segments, seg_seqs)
def run(fp, segment_dir):
    rows = csv.DictReader(open(fp))
    rows = list(rows)[:10]
    results = list(mapcat(F(handle_row, segment_dir), rows))
    head = results[0]
    writer = csv.DictWriter(sys.stdout, fieldnames=head.keys())
    writer.writeheader()
    for result in results:
        writer.writerow(result)
    

def handle_row(segment_dir, row):
    return map(F(row_for_segment, segment_dir, row), segments)

def row_for_segment(segment_dir, old_d, seg): # (str) -> str
   try:
      dir = os.path.join(segment_dir, "*{}*.fasta".format(seg))
      fa = glob(dir)[0]
   except Exception as e:
       log(str(e)+"\n")
       log("fasta file for segment %s not found\n" % seg)
       sys.exit(1)
   seqs = SeqIO.parse(fa, "fasta")
   #log(str(old_d))
   # below, [1:] needed because biopython cuts off '>'
   seq  = find(_.id == old_d['name'][1:], seqs)
   useless = map("segment{}".format, segments) + map("acc{}".format, segments)
   d = dissoc(old_d, *useless)
   # in some version parse on the empty string returns today's date; in another version it throws a value error.
   try:
       date = parse(old_d['date'])
       date_info = dict(month=date.month, day=date.day, year=date.year)
   except ValueError:
       date = None 
   if (date == None) or (date == parse("")):
       date_info = {}
   return merge(d, date_info,
                dict(segment=seg, year=old_d['year'],
                     sequence=str(seq.seq),
                acc=old_d["acc%s" % seg]))


def main():
       fp = sys.argv[1]
       segment_dir = sys.argv[2]
       assert os.path.isdir(segment_dir)
       run(fp, segment_dir)
if __name__ == '__main__': main()       
       
