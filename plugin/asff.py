from GPS import Locations, Menu, File, MDI, Process, Project, Preference
from gs_utils import interactive
import os.path

Preference_Name = "Plugins/Asff/Limit percentage"
Preference(Preference_Name).create("Display matches with scores above the given percentage of the top score", "integer", "20")

class Asff:

    def __init__(self, query):
        self.query = query

    def add_location(self, process, matched, unmatched):
        parts = matched.split(':')
        file_path = parts[0]
        line = parts[1]
        col = parts[2]
        name = parts[3][1:]
        query = self.query
        Locations.add(category="asff: " + query,
                          file=File(file_path),
                          line=int(line),
                          column=int(col),
                          message=name,
                          highlight="Search highlighting",
                          length = len(name))

    def find(self):
        project = os.path.basename(Project.root().file().path)
        regex = "^.+:[0-9]+:[0-9]+:.*$"
        Process (["asff", "-q", self.query, "-P" + project, "--name-only", "--limit-percentage", str(Preference(Preference_Name).get())], regex, on_match=self.add_location)


@interactive(name='open test console',
             menu='/Find/Fuzzy find subprogram')
def open_asff_console():
    query = MDI.input_dialog("Ada Subprogram fuzzy finder", "Query")[0]
    s = Asff(query)
    s.find()
