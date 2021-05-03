#include <iostream>
using namespace std;

#include <boost/variant.hpp>
#include <boost/program_options.hpp>

struct GenericOptions {
    bool debug_;
};

struct LsCommand : public GenericOptions {
    bool hidden_;
    std::string path_;
};

struct ChmodCommand : public GenericOptions {
    bool recurse_;
    std::string perms_;
    std::string path_;
};

class my_visitor : public boost::static_visitor<int> {
public:
    int operator()(LsCommand ls) const {
      cout << ls.path_ << endl;
        return 1;
    }
    
    int operator()(ChmodCommand ch) const {
        return 2;
    }
};

typedef boost::variant<LsCommand, ChmodCommand> Command;

Command ParseOptions(int argc, const char *argv[])
{
    namespace po = boost::program_options;

    bool setHelp = false;

    po::options_description global("Global options");
    global.add_options()
        ("debug", "Turn on debug output")
        ("help", "produce help message")
        ("command", po::value<std::string>(), "command to execute")
        ("subargs", po::value<std::vector<std::string> >(), "Arguments for command");

    po::positional_options_description pos;
    pos.add("command", 1).
        add("subargs", -1);

    po::variables_map vm;

    po::parsed_options parsed = po::command_line_parser(argc, argv).
        options(global).
        positional(pos).
        allow_unregistered().
        run();

    po::store(parsed, vm);

    if (vm.count("help")) {
      setHelp = true;
      cout << "here" << endl;
    }

    cout << "before: " << vm.count("command") << endl;

    if (vm.count("command") == 0) {
        cout << global << "\n";
        throw po::invalid_option_value("dog");
    }

    std::string cmd = vm["command"].as<std::string>();

    cout << "after" << endl;

    if (cmd == "ls")
    {
        // ls command has the following options:
        po::options_description ls_desc("ls options");
        ls_desc.add_options()
            ("hidden", "Show hidden files")
            ("path", po::value<std::string>(), "Path to list")
            ("help", "produce help message")
            ;

        // Collect all the unrecognized options from the first pass. This will include the
        // (positional) command name, so we need to erase that.
        std::vector<std::string> opts = po::collect_unrecognized(parsed.options, po::include_positional);
        opts.erase(opts.begin());

        // Parse again...
        po::store(po::command_line_parser(opts).options(ls_desc).run(), vm);

        if (vm.count("help")) {
          cout << ls_desc << "\n";
          throw po::invalid_option_value("dog");
        }
        
        LsCommand ls;
        ls.debug_ = vm.count("debug");
        ls.hidden_ = vm.count("hidden");
        ls.path_ = vm["path"].as<std::string>();

        return ls;
    }
    else if (cmd == "chmod")
    {
      ChmodCommand ch;
      return ch;
        // Something similar
    }

    // unrecognised command
    throw po::invalid_option_value(cmd);
}

int main(int argc, const char* argv[]) {
  Command c = ParseOptions(argc, argv);
  cout << boost::apply_visitor(my_visitor(), c) << endl;
  //const LsCommand& ls = boost::get<LsCommand>(c);
  //cout << ls.path_ << endl;
}