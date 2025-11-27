% -*- matlab-ts -*-

function indent_xr_ret_after_end(A)

    %{
      Case1:
      (t-utils-xr

      (re-search-forward "end")
      "C-m"
      (insert "%comment"))
    %}
      
    arguments
        A
    end
end
