// Starwisp Copyright (C) 2013 Dave Griffiths
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

package foam.symbai;

import java.util.ArrayList;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.content.Context;
import android.graphics.Color;

import java.io.File;
import java.io.IOException;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import android.widget.TextView;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.Spinner;
import android.widget.ArrayAdapter;
import android.widget.AdapterView;
import android.widget.EditText;
import android.widget.Toast;
import android.view.ViewGroup;
import android.view.ViewGroup.LayoutParams;
import android.view.WindowManager;
import android.view.View;
import android.view.Gravity;
import android.view.KeyEvent;
import android.text.TextWatcher;
import android.text.Editable;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONArray;

import java.util.Calendar;

import foam.starwisp.StarwispActivity;
import foam.starwisp.ActivityManager;
import foam.starwisp.Scheme;
import foam.starwisp.StarwispBuilder;

public class starwisp extends StarwispActivity
{
    static {
        // register all activities here
        ActivityManager.RegisterActivity("main",starwisp.class);
        ActivityManager.RegisterActivity("main2",MainActivity.class);
        ActivityManager.RegisterActivity("village",VillageActivity.class);
        ActivityManager.RegisterActivity("household-list",HouseholdListActivity.class);
        ActivityManager.RegisterActivity("household",HouseholdActivity.class);
        ActivityManager.RegisterActivity("individual",IndividualActivity.class);
        ActivityManager.RegisterActivity("details",DetailsActivity.class);
        ActivityManager.RegisterActivity("family",FamilyActivity.class);
        ActivityManager.RegisterActivity("migration",MigrationActivity.class);
        ActivityManager.RegisterActivity("income",IncomeActivity.class);
        ActivityManager.RegisterActivity("genealogy",GenealogyActivity.class);
        ActivityManager.RegisterActivity("social",SocialActivity.class);
        ActivityManager.RegisterActivity("friendship",FriendshipActivity.class);
        ActivityManager.RegisterActivity("individual-chooser",IndividualChooserActivity.class);
        ActivityManager.RegisterActivity("sync",SyncActivity.class);
        ActivityManager.RegisterActivity("crop",CropActivity.class);
        ActivityManager.RegisterActivity("child",ChildActivity.class);
        ActivityManager.RegisterActivity("move",MoveActivity.class);
        ActivityManager.RegisterActivity("villages",VillagesActivity.class);
        ActivityManager.RegisterActivity("agreement",AgreementActivity.class);
    };

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        String dirname = "symbai/";
        m_AppDir = "/sdcard/"+dirname;
        File appdir = new File(m_AppDir);
        appdir.mkdirs();

        File filesdir = new File(m_AppDir+"/files/");
        filesdir.mkdirs();
        File backupdir = new File(m_AppDir+"/backup/");
        backupdir.mkdirs();

        // build static things
        m_Scheme = new Scheme(this);

        m_Scheme.Load("lib.scm");
        m_Scheme.Load("racket-fix.scm");
        m_Scheme.Load("eavdb/ktv.ss");
        m_Scheme.Load("eavdb/ktv-list.ss");
        m_Scheme.Load("eavdb/entity-values.ss");
        m_Scheme.Load("eavdb/entity-insert.ss");
        m_Scheme.Load("eavdb/entity-get.ss");
        m_Scheme.Load("eavdb/entity-update.ss");
        m_Scheme.Load("eavdb/entity-filter.ss");
        m_Scheme.Load("eavdb/entity-sync.ss");
        m_Scheme.Load("eavdb/entity-csv.ss");
        m_Scheme.Load("eavdb/eavdb.ss");
        m_Scheme.Load("dbsync.scm");

        m_Builder = new StarwispBuilder(m_Scheme);
        m_Name = "main";

        // tell scheme the date
        final Calendar c = Calendar.getInstance();
        int day = c.get(Calendar.DAY_OF_MONTH);
        int month = c.get(Calendar.MONTH)+1;
        int year = c.get(Calendar.YEAR);
        int timezone_offset_mins = (c.get(Calendar.ZONE_OFFSET) + c.get(Calendar.DST_OFFSET)) / 60000;

        String version = "Version not found";
        try {
            PackageInfo pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
            version = pInfo.versionName;
        } catch (NameNotFoundException e) {
            Log.e("starwisp", "Error getting version " + e.toString());
        }

        // pass in a bunch of useful stuff
        m_Scheme.eval("(define dirname \"/sdcard/"+dirname+"\")"+
                      "(define date-day "+day+")"+
                      "(define date-month "+month+")"+
                      "(define date-year "+year+")"+
                      "(define timezone-offset-mins "+timezone_offset_mins+")"+
                      "(define app-version "+version+")");

        Log.i("starwisp","started, now running starwisp.scm...");
        m_Scheme.eval(m_Scheme.readRawTextFile(this, "translations.scm"));
        m_Scheme.eval(m_Scheme.readRawTextFile(this, "starwisp.scm"));

        super.onCreate(savedInstanceState);
    }
}
